{-# LANGUAGE ForeignFunctionInterface #-}

--------------------------------------------------------------------

--------------------------------------------------------------------

-- |
-- Module    : Network.Curl.Easy
-- Copyright : (c) Galois Inc 2007-2009
-- License   :
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Haskell binding to the libcurl <http://curl.haxx.se/> \"easy\" API.
-- The \"easy\" API provides a higher-level, easy-to-get-started calling
-- interface to the library's wide range of features for interacting
-- with HTTP\/FTP\/etc servers.
module Network.Curl.Easy
  ( initialize, -- :: IO Curl
    perform, -- :: Curl -> IO CurlCode
    setopt, -- :: Curl -> CurlOption -> IO CurlCode
    duphandle, -- :: Curl -> IO Curl
    reset, -- :: Curl -> IO ()
    curl_global_init, -- :: CInt -> IO CurlCode
    curl_global_cleanup, -- :: IO ()
    curl_version_number, -- :: IO Int
    curl_version_string, -- :: IO String
  )
where

import Control.Monad
import Data.IORef (IORef)
import Data.Maybe
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr
import Network.Curl.Code
import Network.Curl.Debug
import Network.Curl.Opts
import Network.Curl.Post
import Network.Curl.Types

-- | Initialise a curl instance
initialize :: IO Curl
initialize = do
  h <- easy_initialize
  mkCurl h

-- XXX: Is running cleanup here OK?
reset :: Curl -> IO ()
reset hh = curlPrim hh $ \r h -> easy_reset h >> runCleanup r

duphandle :: Curl -> IO Curl
duphandle hh = curlPrim hh $ \r h ->
  do
    h1 <- easy_duphandle h
    cleanup <- shareCleanup r
    mkCurlWithCleanup h1 cleanup

setopt ::
  Curl ->
  CurlOption ->
  IO CurlCode
setopt hh o = curlPrim hh $ \r h -> unmarshallOption (easy_um r h) o
  where
    easy_um :: IORef OptionMap -> CurlHandle -> Unmarshaller CurlCode
    easy_um r h =
      Unmarshaller
        { long -- :: Int -> Word32     -> IO CurlCode
          =
            \i x -> liftM toCode $ easy_setopt_long h i x,
          llong --  :: Int -> Word64    -> IO CurlCode
          =
            \i x -> liftM toCode $ easy_setopt_llong h i x,
          string -- :: Int -> String   -> IO CurlCode
          =
            \i x -> do
              debug $ "ALLOC: " ++ x
              c_x <- newCString x
              updateCleanup r i $ debug ("FREE: " ++ x) >> free c_x
              liftM toCode $ easy_setopt_string h i c_x,
          strings -- :: Int -> [String] -> IO CurlCode
          =
            \i x ->
              do
                debug ("ALLOC: " ++ show x)
                -- slistAppend will copy its string argument
                let addOne ip s = withCString s $ slistAppend ip
                ip <- foldM addOne nullPtr x
                updateCleanup r i $
                  debug ("FREE: " ++ show x) >> curlSlistFree ip
                liftM toCode $ easy_setopt_string h i (castPtr ip),
          pointer -- :: Int -> Ptr ()   -> IO a
          =
            \i x -> liftM toCode $ easy_setopt_ptr h i x,
          writeFun -- :: Int -> WriteFunction -> IO a
          =
            \i x -> do
              debug "ALLOC: WRITER"
              fp <- mkWriter x
              updateCleanup r i $ debug "FREE: WRITER" >> freeHaskellFunPtr fp
              liftM toCode $ easy_setopt_wfun h i fp,
          readFun -- :: Int -> ReadFunction -> IO a
          =
            \i x -> do
              let wrapResult f a b c d = do
                    mb <- f a b c d
                    return (fromMaybe curlReadfuncAbort mb)
              debug "ALLOC: READER"
              fp <- mkReader (wrapResult x)
              updateCleanup r i $ debug "FREE: READER" >> freeHaskellFunPtr fp
              liftM toCode $ easy_setopt_rfun h i fp,
          progressFun -- :: Int -> ProgressFunction -> IO a
          =
            \i x -> do
              debug "ALLOC: PROGRESS"
              fp <- mkProgress x
              updateCleanup r i $ debug "FREE: PROGRESS" >> freeHaskellFunPtr fp
              liftM toCode $ easy_setopt_fptr h i fp,
          debugFun -- :: Int -> DebugFunction -> IO a
          =
            \i debFun -> do
              let wrapFun fun _a b c d e =
                    fun hh (toEnum (fromIntegral b)) c d e >> return 0
              debug "ALLOC: DEBUG"
              fp <- mkDebugFun (wrapFun debFun)
              updateCleanup r i $ debug "FREE: DEBUG" >> freeHaskellFunPtr fp
              liftM toCode $ easy_setopt_fptr h i fp,
          posts -- :: Int -> [HttpPost] -> IO a
          =
            \i x -> do
              debug "ALLOC: POSTS"
              p <- marshallPosts x
              updateCleanup r i $ debug "FREE: POSTS" >> curlFormfree p
              liftM toCode $ easy_setopt_ptr h i p,
          sslctxt -- :: Int -> SSLCtxtFunction -> IO a
          =
            \i x -> do
              debug "ALLOC: SSL_FUN"
              p <- mkSslCtxtFun x
              updateCleanup r i $ debug "FREE: SSL_FUN" >> freeHaskellFunPtr p
              liftM toCode $ easy_setopt_fptr h i p,
          ioctlFun -- :: Int -> Ptr () -> IO a
          =
            \i x -> liftM toCode $ easy_setopt_ptr h i x,
          convFromNetwork -- :: Int -> Ptr () -> IO a
          =
            \i x -> liftM toCode $ easy_setopt_ptr h i x,
          convToNetwork -- :: Int -> Ptr () -> IO a
          =
            \i x -> liftM toCode $ easy_setopt_ptr h i x,
          convFromUtf8 -- :: Int -> Ptr () -> IO a
          =
            \i x -> liftM toCode $ easy_setopt_ptr h i x,
          sockoptFun -- :: Int -> Ptr () -> IO a
          =
            \i x -> liftM toCode $ easy_setopt_ptr h i x
        }

perform :: Curl -> IO CurlCode
perform hh = liftM toCode $ curlPrim hh $ \_ h -> easy_perform_prim h

curl_global_init :: CInt -> IO CurlCode
curl_global_init v = liftM toCode $ curl_global_init_prim v

curl_version_number :: IO Int
curl_version_number = do
  x <- curl_version_num
  return (fromIntegral x)

curl_version_string :: IO String
curl_version_string = do
  cs <- curl_version_str
  peekCString cs

-- FFI decls

foreign import ccall "curl_version_num"
  curl_version_num :: IO CInt

foreign import ccall "curl_version_str"
  curl_version_str :: IO CString

foreign import ccall "curl/easy.h curl_global_init"
  curl_global_init_prim :: CInt -> IO CInt

foreign import ccall "curl/easy.h curl_global_cleanup"
  curl_global_cleanup :: IO ()

foreign import ccall "curl/easy.h curl_easy_init"
  easy_initialize :: IO CurlHandle

foreign import ccall "curl/easy.h curl_easy_perform"
  easy_perform_prim :: CurlHandle -> IO CInt

foreign import ccall "curl_easy_duphandle"
  easy_duphandle :: CurlHandle -> IO CurlHandle

foreign import ccall "curl_easy_reset"
  easy_reset :: CurlHandle -> IO ()

foreign import ccall "curl_easy_setopt_long"
  easy_setopt_long :: CurlHandle -> Int -> Word32 -> IO CInt

foreign import ccall "curl_easy_setopt_longlong"
  easy_setopt_llong :: CurlHandle -> Int -> Word64 -> IO CInt

foreign import ccall "curl_easy_setopt_string"
  easy_setopt_string :: CurlHandle -> Int -> Ptr CChar -> IO CInt

foreign import ccall "curl_easy_setopt_ptr"
  easy_setopt_ptr :: CurlHandle -> Int -> Ptr a -> IO CInt

foreign import ccall "curl_easy_setopt_ptr"
  easy_setopt_fptr :: CurlHandle -> Int -> FunPtr a -> IO CInt

foreign import ccall "curl_easy_setopt_ptr"
  easy_setopt_wfun :: CurlHandle -> Int -> FunPtr WriteFunction -> IO CInt

foreign import ccall "curl_easy_setopt_ptr"
  easy_setopt_rfun :: CurlHandle -> Int -> FunPtr ReadFunctionPrim -> IO CInt

foreign import ccall "wrapper"
  mkWriter :: WriteFunction -> IO (FunPtr WriteFunction)

foreign import ccall "wrapper"
  mkReader :: ReadFunctionPrim -> IO (FunPtr ReadFunctionPrim)

foreign import ccall "wrapper"
  mkProgress :: ProgressFunction -> IO (FunPtr ProgressFunction)

foreign import ccall "wrapper"
  mkDebugFun :: DebugFunctionPrim -> IO (FunPtr DebugFunctionPrim)

foreign import ccall "wrapper"
  mkSslCtxtFun :: SSLCtxtFunction -> IO (FunPtr SSLCtxtFunction)
