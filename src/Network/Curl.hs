{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module    : Network.Curl
-- Copyright : (c) 2007-2009, Galois Inc
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- A Haskell binding the libcurl library <http://curl.haxx.se/>, a
-- proven and feature-rich library for interacting with HTTP(S)\/FTP
-- servers.
--
-- The binding was initially made against version 7.16.2; libcurl does
-- appear to be considerate in not introducing breaking changes wrt
-- older versions. So, unless you're after the latest features (i.e.,
-- constructors towards the end the Option type), there's a very good
-- chance your code will work against older installations of libcurl.
module Network.Curl where

import Control.Exception (finally)
import Control.Monad (void, when, (>=>))
import Control.Monad.Catch (MonadThrow (throwM))
import Data.ByteString (packCStringLen)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Data.Foldable (foldl', traverse_)
import Data.Functor ((<&>))
import Data.IORef
import Data.List (isPrefixOf)
import Foreign.C (CInt)
import Foreign.C.String
import Network.Curl.Easy
import Network.Curl.Info
import Network.Curl.Opts
import Network.Curl.Post
import Network.Curl.Types

newIncomingHeader :: IO (IO (String, [(String, String)]), CStringLen -> IO ())
newIncomingHeader = do
  newIORef [] <&> \ref ->
    (readFinalHeader ref, peekCStringLen >=> (modifyIORef ref . (:)))
  where
    readFinalHeader :: IORef [String] -> IO (String, [(String, String)])
    readFinalHeader = fmap (parseStatusNHeaders . concatReverse []) . readIORef

newIncomingBuffer :: IO (IO ByteString, CStringLen -> IO ())
newIncomingBuffer =
  newIORef [] <&> \ref ->
    ( readIORef ref <&> (Lazy.ByteString.fromChunks . reverse),
      packCStringLen >=> (modifyIORef ref . (:))
    )

-- | Should be used once to wrap all uses of libcurl.
-- WARNING: the argument should not pure before it
-- is completely done with curl (e.g., no forking or lazy returns)
withCurlDo :: IO a -> IO a
withCurlDo m = do
  void $ curlGlobalInit 3 -- initialize everything
  finally m curlGlobalCleanup

-- | Set a list of options on a Curl handle.
setopts :: Curl -> [CurlOption] -> IO ()
setopts curl = traverse_ (setopt curl)

-- | 'curlGet' perform a basic GET, dumping the output on stdout.
-- The list of options are set prior performing the GET request.
curlGet :: UrlString -> [CurlOption] -> IO ()
curlGet url opts =
  initialize >>= \curl -> do
    void $ setopt curl (FailOnError True)
    void $ setopt curl (Url url)
    -- Note: later options may (and should, probably) override these defaults.
    setDefaultSslOpts curl url
    traverse_ (setopt curl) opts
    void $ perform curl

setDefaultSslOpts :: Curl -> UrlString -> IO ()
setDefaultSslOpts curl url =
  when ("https:" `isPrefixOf` url) $
    -- the default options are pretty dire, really -- turning off
    -- the peer verification checks!
    traverse_
      (setopt curl)
      [ SslVerifyPeer False,
        SslVerifyHost 0
      ]

curlGetString :: UrlString -> [CurlOption] -> IO (CurlCode, ByteString)
curlGetString url opts =
  initialize >>= \curl -> do
    (finalBody, gatherBody) <- newIncomingBuffer
    void . setopt curl $ FailOnError True
    setDefaultSslOpts curl url
    void . setopt curl $ Url url
    void . setopt curl . WriteFunc $ callbackWriter_ gatherBody
    traverse_ (setopt curl) opts
    (,) <$> perform curl <*> finalBody

-- | 'CurlResponse' is a record type encoding all the information
-- embodied in a response to your Curl request. Currently only used
-- to gather up the results of doing a GET in 'curlGetResponse'.
data CurlResponse = CurlResponse
  { curlCode :: CurlCode,
    status :: Int,
    statusLine :: String,
    headers :: [(String, String)],
    body :: ByteString,
    info :: Info -> IO InfoValue
  }

-- | @curlGetResponse url opts@ performs a @GET@, returning all the info
-- it can lay its hands on in the response, a value of type 'CurlResponse'.
-- The representation of the body is overloaded
curlGetResponse :: UrlString -> [CurlOption] -> IO CurlResponse
curlGetResponse url opts = do
  curl <- initialize
  -- Note: later options may (and should, probably) override these defaults.
  void . setopt curl $ FailOnError True
  setDefaultSslOpts curl url
  void . setopt curl $ Url url
  traverse_ (setopt curl) opts
  -- note that users cannot over-write the body and header handler
  -- which makes sense because otherwise we will pure a bogus reposnse.
  performWithResponse curl

-- | Perform the actions already specified on the handle.
-- Collects useful information about the returned message.
-- Note that this function sets the
-- 'CurlWriteFunction' and 'CurlHeaderFunction' options.
-- The returned payload is overloaded over the representation of
-- both headers and body via the 'CurlResponse_' type.
performWithResponse :: Curl -> IO CurlResponse
performWithResponse curl = do
  (finalHeader, gatherHeader) <- newIncomingHeader
  (finalBody, gatherBody) <- newIncomingBuffer
  -- Instead of allocating a separate handler for each
  -- request we could just set this options one and forall
  -- and just clear the IORefs.
  void . setopt curl . WriteFunc $ callbackWriter_ gatherBody
  void . setopt curl . HeaderFunc $ callbackWriter_ gatherHeader
  rc <- perform curl
  rspCode <- getResponseCode curl
  (st, hs) <- finalHeader
  bs <- finalBody
  pure
    CurlResponse
      { curlCode = rc,
        status = rspCode,
        statusLine = st,
        headers = hs,
        body = bs,
        -- note: we're holding onto the handle here..
        -- note: with this interface this is not neccessary.
        info = getInfo curl
      }

doCurl :: Curl -> UrlString -> [CurlOption] -> IO CurlResponse
doCurl curl url opts = do
  setDefaultSslOpts curl url
  setopts curl opts
  void . setopt curl $ Url url
  performWithResponse curl

-- | Get the headers associated with a particular URL.
-- Returns the status line and the key-value pairs for the headers.
curlHead :: UrlString -> [CurlOption] -> IO (String, [(String, String)])
curlHead url opts =
  initialize >>= \curl ->
    do
      ref <- newIORef []
      --     setopt curl (Verbose True)
      void $ setopt curl $ Url url
      void $ setopt curl $ NoBody True
      traverse_ (setopt curl) opts
      void . setopt curl . HeaderFunc $ gatherOutput ref
      void $ perform curl
      lss <- readIORef ref
      pure (parseStatusNHeaders (concatReverse [] lss))

-- | Get the headers associated with a particular URL.
-- Returns the status line and the key-value pairs for the headers.
curlHead_ :: UrlString -> [CurlOption] -> IO (String, [(String, String)])
curlHead_ url opts =
  initialize >>= \curl -> do
    (finalHeader, gatherHeader) <- newIncomingHeader
    void $ setopt curl (Url url)
    void $ setopt curl (NoBody True)
    traverse_ (setopt curl) opts
    void . setopt curl $ HeaderFunc $ callbackWriter_ gatherHeader
    void $ perform curl
    finalHeader

-- utils

concatReverse :: [a] -> [[a]] -> [a]
concatReverse = foldl' (flip (<>))

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

-- | 'curlMultiPost' perform a multi-part POST submission.
curlMultiPost :: UrlString -> [CurlOption] -> [HttpPost] -> IO ()
curlMultiPost s os ps =
  initialize >>= \curl -> do
    void $ setopt curl (Verbose True)
    void $ setopt curl (Url s)
    void $ setopt curl (Multipart ps)
    traverse_ (setopt curl) os
    void $ perform curl

-- | 'curlPost' performs. a common POST operation, namely that
-- of submitting a sequence of name=value pairs.
curlPost :: UrlString -> [String] -> IO ()
curlPost s ps =
  initialize >>= \curl -> do
    void . setopt curl $ Verbose True
    void . setopt curl $ PostFields ps
    void . setopt curl $ CookieJar "cookies"
    void . setopt curl $ Url s
    void $ perform curl

-- Use 'callbackWriter' instead.

easyWriter :: (String -> IO ()) -> WriteFunction
easyWriter = callbackWriter

-- | Imports data into the Haskell world and invokes the callback.
callbackWriter :: (String -> IO ()) -> WriteFunction
callbackWriter f = WriteFunction $ \buf width num _ -> do
  let bytes :: CInt
      bytes = width * num
  f =<< peekCStringLen (buf, fromIntegral bytes)
  pure bytes

-- | Imports data into the Haskell world and invokes the callback.
callbackWriter_ :: (CStringLen -> IO ()) -> WriteFunction
callbackWriter_ f = WriteFunction $ \buf width num _ -> do
  let bytes :: CInt
      bytes = width * num
  f (buf, fromIntegral bytes)
  pure bytes

-- | The output of Curl is ignored.  This function
-- does not marshall data into Haskell.
ignoreOutput :: WriteFunction
ignoreOutput = WriteFunction $ \_ x y _ -> pure $ x * y

-- | Add chunks of data to an IORef as they arrive.
gatherOutput :: IORef [String] -> WriteFunction
gatherOutput r = callbackWriter $ modifyIORef r . (:)

getResponseCode :: Curl -> IO Int
getResponseCode c =
  getInfo c ResponseCode >>= \case
    String s -> case reads s of
      ((v, _) : _) -> pure v
      _ -> throwM $ InvalidResponse s
    Double d -> pure $ round d
    Long l -> pure $ fromIntegral l
    iv@(List _) -> throwM $ UnexpectedResponse iv
