--------------------------------------------------------------------

--------------------------------------------------------------------

-- |
-- Module    : Network.Curl.Info
-- Copyright : (c) 2007-2009, Galois Inc
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Accessing the properties of a curl handle's current state\/request.
module Network.Curl.Info
  ( getInfo,
  )
where

import Control.Monad.Catch (MonadThrow (throwM))
import Data.Word (Word32)
import Foreign.C (CChar, CInt (CInt), CString, peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (peek, peekByteOff, sizeOf))
import Network.Curl.Types

getInfo :: Curl -> Info -> IO InfoValue
getInfo curl = \case
  EffectiveUrl -> getInfoStr curl 1
  ResponseCode -> getInfoLong curl 2
  TotalTime -> getInfoDouble curl 3
  NameLookupTime -> getInfoDouble curl 4
  ConnectTime -> getInfoDouble curl 5
  PreTransferTime -> getInfoDouble curl 6
  SizeUpload -> getInfoDouble curl 7
  SizeDownload -> getInfoDouble curl 8
  SpeedDownload -> getInfoDouble curl 9
  SpeedUpload -> getInfoDouble curl 10
  HeaderSize -> getInfoLong curl 11
  RequestSize -> getInfoLong curl 12
  SslVerifyResult -> getInfoLong curl 13
  Filetime -> getInfoLong curl 14
  ContentLengthDownload -> getInfoDouble curl 15
  ContentLengthUpload -> getInfoDouble curl 16
  StartTransferTime -> getInfoDouble curl 17
  ContentType -> getInfoStr curl 18
  RedirectTime -> getInfoDouble curl 19
  RedirectCount -> getInfoLong curl 20
  Private -> getInfoStr curl 21
  HttpConnectCode -> getInfoLong curl 22
  HttpAuthAvail -> getInfoLong curl 23
  ProxyAuthAvail -> getInfoLong curl 24
  OSErrno -> getInfoLong curl 25
  NumConnects -> getInfoLong curl 26
  SslEngines -> getInfoSList curl 27
  CookieList -> getInfoSList curl 28
  LastSocket -> getInfoLong curl 29
  FtpEntryPath -> getInfoStr curl 30

getInfoStr :: Curl -> Word32 -> IO InfoValue
getInfoStr curl tg =
  alloca $ \ptr ->
    act ptr >>= \case
      CurlExitSuccess ->
        peek ptr >>= \case
          s
            | s == nullPtr -> pure $ String mempty
            | otherwise -> String <$> peekCString s
      rc -> throwM $ codeFromCInt rc
  where
    act :: Ptr CString -> IO CInt
    act ptr = curlPrim curl . const $ \p -> easyGetinfoStr p tg ptr

getInfoLong :: Curl -> Word32 -> IO InfoValue
getInfoLong curl tg =
  alloca $ \ptr ->
    act ptr >>= \case
      CurlExitSuccess -> Long <$> peek ptr
      rc -> throwM $ codeFromCInt rc
  where
    act :: Ptr Word32 -> IO CInt
    act ptr = curlPrim curl . const $ \p -> easyGetinfoLong p tg ptr

getInfoDouble :: Curl -> Word32 -> IO InfoValue
getInfoDouble curl tg =
  alloca $ \ptr ->
    act ptr >>= \case
      CurlExitSuccess -> Double <$> peek ptr
      rc -> throwM $ codeFromCInt rc
  where
    act :: Ptr Double -> IO CInt
    act ptr = curlPrim curl . const $ \p -> easyGetinfoDouble p tg ptr

getInfoSList :: Curl -> Word32 -> IO InfoValue
getInfoSList curl tg =
  alloca $ \ptr ->
    act ptr >>= \case
      CurlExitSuccess -> fmap List . unmarshallList =<< peek ptr
      rc -> throwM $ codeFromCInt rc
  where
    act :: Ptr (Ptr (Ptr CChar)) -> IO CInt
    act ptr = curlPrim curl . const $ \p -> easyGetinfoSlist p tg ptr

    unmarshallList :: Ptr (Ptr CChar) -> IO [String]
    unmarshallList ptr
      | ptr == nullPtr = pure []
      | otherwise = do
          s <-
            peekByteOff ptr 0 >>= \case
              ps
                | ps == nullPtr -> pure mempty
                | otherwise -> peekCString ps
          fmap (s :) . unmarshallList =<< peekByteOff ptr (sizeOf nullPtr)

-- FFI decls
foreign import ccall "curl_easy_getinfo_long"
  easyGetinfoLong :: CurlHandle -> Word32 -> Ptr Word32 -> IO CInt

foreign import ccall "curl_easy_getinfo_string"
  easyGetinfoStr :: CurlHandle -> Word32 -> Ptr CString -> IO CInt

foreign import ccall "curl_easy_getinfo_double"
  easyGetinfoDouble :: CurlHandle -> Word32 -> Ptr Double -> IO CInt

foreign import ccall "curl_easy_getinfo_slist"
  easyGetinfoSlist :: CurlHandle -> Word32 -> Ptr (Ptr (Ptr CChar)) -> IO CInt
