module Network.Curl.Internal where

import Control.Monad (void, when, (>=>))
import Control.Monad.Catch (MonadThrow (throwM))
import Data.ByteString (packCStringLen)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Data.Foldable (foldl', traverse_)
import Data.Functor ((<&>))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (isPrefixOf)
import Foreign.C (CInt, CStringLen, peekCStringLen)
import Network.Curl.Easy
import Network.Curl.Info
import Network.Curl.Opts
import Network.Curl.Post
import Network.Curl.Types

runWithResponse :: UrlString -> [CurlOption] -> Curl -> IO CurlResponse
runWithResponse url opts curl = do
  setDefaultSslOpts curl url
  setopts curl opts
  setopt curl $ Url url
  performWithResponse curl

-- | 'curlGet' perform a basic GET, dumping the output on stdout.
-- The list of options are set prior performing the GET request.
curlGet :: UrlString -> [CurlOption] -> Curl -> IO ()
curlGet url opts curl = do
  setopts curl [FailOnError True, Url url]
  -- Note: later options may (and should, probably) override these defaults.
  setDefaultSslOpts curl url
  setopts curl opts
  void $ perform curl

curlGetString :: UrlString -> [CurlOption] -> Curl -> IO (CurlCode, ByteString)
curlGetString url opts curl = do
  (finalBody, gatherBody) <- newIncomingBuffer
  setDefaultSslOpts curl url
  setopts
    curl
    [ FailOnError True,
      Url url,
      WriteFun $ callbackWriter gatherBody
    ]
  setopts curl opts
  (,) <$> perform curl <*> finalBody

-- | Get the headers associated with a particular URL.
-- Returns the status line and the key-value pairs for the headers.
curlHead :: UrlString -> [CurlOption] -> Curl -> IO (String, [(String, String)])
curlHead url opts curl = do
  (finalHeader, gatherHeader) <- newIncomingHeader
  setopts
    curl
    [ Url url,
      NoBody True,
      HeadFun $ callbackWriter gatherHeader
    ]
  setopts curl opts
  void $ perform curl
  finalHeader

-- | 'curlPost' performs. a common POST operation, namely that
-- of submitting a sequence of name=value pairs.
curlPost :: UrlString -> [String] -> Curl -> IO ()
curlPost s ps curl = do
  setopts curl [Verbose True, PostFields ps, CookieJar "cookies", Url s]
  void $ perform curl

-- | 'curlMultiPost' perform a multi-part POST submission.
curlMultipart :: UrlString -> [CurlOption] -> [HttpPost] -> Curl -> IO ()
curlMultipart s os ps curl = do
  setopts curl [Verbose True, Url s, Multipart ps]
  setopts curl os
  void $ perform curl

newIncomingHeader :: IO (IO (String, [(String, String)]), CStringLen -> IO ())
newIncomingHeader = do
  newIORef [] <&> \ref ->
    (readFinalHeader ref, peekCStringLen >=> (modifyIORef ref . (:)))
  where
    readFinalHeader :: IORef [String] -> IO (String, [(String, String)])
    readFinalHeader =
      fmap (parseStatusNHeaders . foldl' (flip (<>)) []) . readIORef

newIncomingBuffer :: IO (IO ByteString, CStringLen -> IO ())
newIncomingBuffer =
  newIORef [] <&> \ref ->
    ( readIORef ref <&> (Lazy.ByteString.fromChunks . reverse),
      packCStringLen >=> (modifyIORef ref . (:))
    )

-- | Set a list of options on a Curl handle.
setopts :: Curl -> [CurlOption] -> IO ()
setopts curl = traverse_ $ setopt curl

setDefaultSslOpts :: Curl -> UrlString -> IO ()
setDefaultSslOpts curl url =
  when ("https:" `isPrefixOf` url) $
    -- the default options are pretty dire, really -- turning off
    -- the peer verification checks!
    setopts curl [SslVerifyPeer False, SslVerifyHost 0]

-- | Perform the actions already specified on the handle.
-- Collects useful information about the returned message.
-- Note that this function sets the
-- 'WriteFun' and 'HeadFun' options
performWithResponse :: Curl -> IO CurlResponse
performWithResponse curl = do
  (finalHeader, gatherHeader) <- newIncomingHeader
  (finalBody, gatherBody) <- newIncomingBuffer
  -- Instead of allocating a separate handler for each
  -- request we could just set this options one and forall
  -- and just clear the IORefs.
  setopts
    curl
    [ WriteFun $ callbackWriter gatherBody,
      HeadFun $ callbackWriter gatherHeader
    ]
  curlCode <- perform curl
  status <- getResponseCode curl
  (statusLine, headers) <- finalHeader
  body <- finalBody
  pure
    CurlResponse
      { curlCode,
        status,
        headers,
        statusLine,
        body,
        -- note: we're holding onto the handle here..
        -- note: with this interface this is not neccessary.
        info = getInfo curl
      }

-- utils

parseStatusNHeaders :: String -> (String, [(String, String)])
parseStatusNHeaders ys = case intoLines [] ys of
  a : as -> (a, map parseHeader as)
  [] -> (mempty, [])
  where
    intoLines :: String -> String -> [String]
    intoLines acc = \case
      "" -> addLine acc []
      ('\r' : '\n' : xs) -> addLine acc $ intoLines mempty xs
      (x : xs) -> intoLines (x : acc) xs

    addLine :: String -> [String] -> [String]
    addLine "" ls = ls
    addLine l ls = reverse l : ls

parseHeader :: String -> (String, String)
parseHeader xs = case break (':' ==) xs of
  (as, _ : bs) -> (as, bs)
  (as, _) -> (as, mempty)

-- | Imports data into the Haskell world and invokes the callback.
callbackWriter :: (CStringLen -> IO ()) -> WriteFunction
callbackWriter f = WriteFunction $ \buf width num _ -> do
  let bytes :: CInt
      bytes = width * num
  bytes <$ f (buf, fromIntegral bytes)

-- | The output of Curl is ignored.  This function
-- does not marshall data into Haskell.
ignoreOutput :: WriteFunction
ignoreOutput = WriteFunction $ \_ x y _ -> pure $ x * y

getResponseCode :: Curl -> IO Int
getResponseCode c =
  getInfo c ResponseCode >>= \case
    String s -> case reads s of
      ((v, _) : _) -> pure v
      _ -> throwM $ InvalidResponse s
    Double d -> pure $ round d
    Long l -> pure $ fromIntegral l
    iv@(List _) -> throwM $ UnexpectedResponse iv
