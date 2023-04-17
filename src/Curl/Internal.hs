{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}

module Curl.Internal where

import Control.Monad (when, (>=>))
import Control.Monad.Catch (MonadThrow (throwM))
import Curl.Internal.Easy
import Curl.Internal.Info
import Curl.Internal.Opts
import Curl.Internal.Post
import Curl.Internal.Types
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.CaseInsensitive as CaseInsensitive
import Data.Foldable (foldl', traverse_)
import Data.Functor ((<&>))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as Map
import Data.Traversable (for)
import Foreign.C (CInt, CStringLen)
import Network.HTTP.Types (Header)
import qualified URI.ByteString

runWithResponse :: Url -> [CurlOption] -> Curl -> IO CurlResponse
runWithResponse url opts = runWithResponseInfo url opts mempty

runWithResponseInfo ::
  Url -> [CurlOption] -> [Info] -> Curl -> IO CurlResponse
runWithResponseInfo url opts infos curl = do
  setDefaultSslOpts curl url
  setPort curl url
  setopts curl opts
  setopt curl $ UseUrl url
  performWithResponse curl infos

-- | 'curlGet' perform a basic GET, dumping the output on stdout.
-- The list of options are set prior performing the GET request.
curlGet :: Url -> [CurlOption] -> Curl -> IO CurlCode
curlGet url opts curl = do
  setopts curl [FailOnError True, UseUrl url]
  -- Note: later options may (and should, probably) override these defaults.
  setDefaultSslOpts curl url
  setPort curl url
  setopts curl opts
  perform curl

curlGetString ::
  Url ->
  [CurlOption] ->
  Curl ->
  IO (CurlCode, Lazy.ByteString.ByteString)
curlGetString url opts curl = do
  (finalBody, gatherBody) <- newIncomingBuffer
  setDefaultSslOpts curl url
  setPort curl url
  setopts
    curl
    [ FailOnError True,
      UseUrl url,
      WriteFun $ callbackWriter gatherBody
    ]
  setopts curl opts
  (,) <$> perform curl <*> finalBody

-- | Get the headers associated with a particular URL.
-- Returns 'CurlResponse' with relevant information
curlHead :: Url -> [CurlOption] -> Curl -> IO CurlResponse
curlHead url opts curl = do
  (finalHeader, gatherHeader) <- newIncomingHeader
  setopts
    curl
    [ UseUrl url,
      NoBody True,
      HeadFun $ callbackWriter gatherHeader
    ]
  setopts curl opts
  curlCode <- perform curl
  status <- getResponseCode curl
  (statusLine, headers) <- finalHeader
  pure
    CurlResponse
      { curlCode,
        status,
        headers,
        statusLine,
        body = mempty,
        info = Map.empty
      }

-- | 'curlPost' performs. a common POST operation, namely that
-- of submitting a sequence of name=value pairs.
curlPost :: Url -> [String] -> Curl -> IO CurlCode
curlPost s ps curl = do
  setopts curl [Verbose True, PostFields ps, CookieJar "cookies", UseUrl s]
  perform curl

-- | 'curlMultiPost' perform a multi-part POST submission.
curlMultipart :: Url -> [CurlOption] -> [HttpPost] -> Curl -> IO CurlCode
curlMultipart s os ps curl = do
  setopts curl [Verbose True, UseUrl s, Multipart ps]
  setopts curl os
  perform curl

newIncomingHeader :: IO (IO (ByteString, [Header]), CStringLen -> IO ())
newIncomingHeader = do
  newIORef [] <&> \ref ->
    (readFinalHeader ref, ByteString.packCStringLen >=> (modifyIORef ref . (:)))
  where
    readFinalHeader ::
      IORef [ByteString] -> IO (ByteString, [Header])
    readFinalHeader =
      fmap (parseStatusHeaders . foldl' (flip (<>)) mempty) . readIORef

newIncomingBuffer :: IO (IO Lazy.ByteString.ByteString, CStringLen -> IO ())
newIncomingBuffer =
  newIORef [] <&> \ref ->
    ( readIORef ref <&> (Lazy.ByteString.fromChunks . reverse),
      ByteString.packCStringLen >=> (modifyIORef ref . (:))
    )

-- | Set a list of options on a Curl handle.
setopts :: Curl -> [CurlOption] -> IO ()
setopts curl = traverse_ $ setopt curl

setDefaultSslOpts :: Curl -> Url -> IO ()
setDefaultSslOpts
  curl
  (Url (URI.ByteString.URI (URI.ByteString.Scheme scheme) _ _ _ _)) =
    when (scheme == "https") $
      setopts curl [SslVerifyPeer True, SslVerifyHost 1]

setPort :: Curl -> Url -> IO ()
setPort curl (Url url)
  | URI.ByteString.URI _ (Just authority) _ _ _ <- url,
    URI.ByteString.Authority _ _ (Just (URI.ByteString.Port port)) <- authority =
      setopt curl . Port $ fromIntegral port
  | otherwise = pure ()

-- | Perform the actions already specified on the handle.
-- Collects useful information about the returned message.
-- Note that this function sets the
-- 'WriteFun' and 'HeadFun' options
performWithResponse :: Curl -> [Info] -> IO CurlResponse
performWithResponse curl infos = do
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
  info <- fmap Map.fromList . for infos $ \i -> (i,) <$> getInfo curl i
  pure
    CurlResponse
      { curlCode,
        status,
        headers,
        statusLine,
        body,
        info
      }

-- utils

parseStatusHeaders :: ByteString -> (ByteString, [Header])
parseStatusHeaders bs = case rlines bs of
  status : hs -> (status, mkHeaders hs)
  _ -> (bs, mempty)
  where
    mkHeaders :: [ByteString] -> [Header]
    mkHeaders =
      fmap (first CaseInsensitive.mk . parseHeader)
        . filter (not . ByteString.Char8.null)
    -- Split the header string by carriage returns. The first element is the
    -- status line, followed by the headers
    rlines :: ByteString -> [ByteString]
    rlines ps
      | ByteString.Char8.null ps = mempty
      | otherwise =
          ByteString.Char8.strip <$> case ByteString.Char8.elemIndex '\r' ps of
            Nothing -> [ps]
            Just n ->
              ByteString.Char8.take n ps
                : rlines (ByteString.Char8.drop (n + 1) ps)

    parseHeader :: ByteString -> (ByteString, ByteString)
    parseHeader ps =
      bimap ByteString.Char8.strip ByteString.Char8.strip $
        case ByteString.Char8.break (':' ==) ps of
          (x, y)
            | Just (_, z) <- ByteString.Char8.uncons y -> (x, z)
            | otherwise -> (x, mempty)

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
