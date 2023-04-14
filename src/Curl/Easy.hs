--------------------------------------------------------------------

--------------------------------------------------------------------

-- |
-- Module    : Curl.Easy
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
module Curl.Easy where

import Control.Exception (finally)
import Control.Monad (foldM)
import Control.Monad.Catch (mask_)
import Data.IORef (IORef)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.C.Types (CChar, CInt (CInt))
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Curl.Opts
import Curl.Post
import Curl.Types

-- | Should be used once to wrap all uses of libcurl.
-- WARNING: the argument should not pure before it
-- is completely done with curl (e.g., no forking or lazy returns)
withCurlDo :: IO a -> IO a
withCurlDo m = do
  withCheckCurlCode $ curlGlobalInit 3 -- initialize everything
  finally m curlGlobalCleanup

runCurl :: (Curl -> IO a) -> IO a
runCurl f = do
  -- Initialize global libcurl instance
  -- This can be done automatically using `curl_easy_init`, but
  -- the curl docs recommend avoiding the implicit route
  withCheckCurlCode $ curlGlobalInit 3
  finally (f =<< initialize) curlGlobalCleanup

-- | Initialise a curl instance
initialize :: IO Curl
initialize = mask_ . mkCurl =<< easyInitialize

setopt :: Curl -> CurlOption -> IO ()
setopt curl o = mask_ $ withCheckCurlCode doPrim
  where
    doPrim :: IO CurlCode
    doPrim = curlPrim curl $ \r h -> unmarshallOption (easyUm r h) o

    easyUm :: IORef OptionMap -> CurlHandle -> Unmarshaller CurlCode
    easyUm r h =
      Unmarshaller
        { -- :: Int -> Word32     -> IO CurlCode
          long = \i x -> codeFromCInt <$> easySetoptLong h i x,
          --  :: Int -> Word64    -> IO CurlCode
          llong = \i x -> codeFromCInt <$> easySetoptLLong h i x,
          -- :: Int -> String   -> IO CurlCode
          string = \i x -> do
            cstr <- newCString x
            updateCleanup r i $ free cstr
            codeFromCInt <$> easySetoptString h i cstr,
          -- :: Int -> [String] -> IO CurlCode
          strings = \i x -> do
            -- slistAppend will copy its string argument
            let addOne :: Ptr Slist -> String -> IO (Ptr Slist)
                addOne ip s = withCString s $ slistAppend ip
            ip <- foldM addOne nullPtr x
            updateCleanup r i $ curlSlistFree ip
            codeFromCInt <$> easySetoptString h i (castPtr ip),
          -- :: Int -> Ptr () -> IO a
          pointer = \i x -> codeFromCInt <$> easySetoptPtr h i x,
          -- :: Int -> WriteFunction -> IO a
          writeFun = \i x -> do
            fp <- mkWriter x
            updateCleanup r i $ freeHaskellFunPtr fp
            codeFromCInt <$> easySetoptWfun h i fp,
          -- :: Int -> ReadFunction -> IO a
          readFun = \i readFun -> do
            fp <- mkReader $ readToPrim readFun
            updateCleanup r i $ freeHaskellFunPtr fp
            codeFromCInt <$> easySetoptRfun h i fp,
          -- :: Int -> ProgressFunction -> IO a
          progressFun = \i x -> do
            fp <- mkProgress x
            updateCleanup r i $ freeHaskellFunPtr fp
            codeFromCInt <$> easySetoptFptr h i fp,
          -- :: Int -> DebugFunction -> IO a
          debugFun = \i dbgFun -> do
            fp <- mkDebugFun $ debugToPrim dbgFun
            updateCleanup r i $ freeHaskellFunPtr fp
            codeFromCInt <$> easySetoptFptr h i fp,
          -- :: Int -> [HttpPost] -> IO a
          posts = \i x -> do
            p <- marshallPosts x
            updateCleanup r i $ curlFormfree p
            codeFromCInt <$> easySetoptPtr h i p,
          -- :: Int -> SSLCtxtFunction -> IO a
          sslctxt = \i x -> do
            p <- mkSslCtxtFun x
            updateCleanup r i $ freeHaskellFunPtr p
            codeFromCInt <$> easySetoptFptr h i p,
          -- :: Int -> Ptr () -> IO a
          ioctlFun = fptr h,
          -- :: Int -> Ptr () -> IO a
          convFromNetwork = fptr h,
          -- :: Int -> Ptr () -> IO a
          convToNetwork = fptr h,
          -- :: Int -> Ptr () -> IO a
          convFromUtf8 = fptr h,
          -- :: Int -> Ptr () -> IO a
          sockoptFun = fptr h
        }

    fptr :: CurlHandle -> Int -> Ptr a -> IO CurlCode
    fptr h i = fmap codeFromCInt . easySetoptPtr h i

    readToPrim :: ReadFunction -> ReadFunctionPrim
    readToPrim (ReadFunction f) a b c d =
      fromMaybe curlReadfuncAbort <$> f a b c d

    debugToPrim :: DebugFunction -> DebugFunctionPrim
    debugToPrim (DebugFunction f) _ b c d e =
      0 <$ f curl (toEnum (fromIntegral b)) c d e

perform :: Curl -> IO CurlCode
perform curl = codeFromCInt <$> curlPrim curl (const easyPerformPrim)

curlGlobalInit :: CInt -> IO CurlCode
curlGlobalInit v = codeFromCInt <$> curlGlobalInitPrim v

curlVersionNumber :: IO Int
curlVersionNumber = fromIntegral <$> curlVersionNum

curlVersionString :: IO String
curlVersionString = peekCString =<< curlVersionStr

-- FFI decls

foreign import ccall "curl_version_num"
  curlVersionNum :: IO CInt

foreign import ccall "curl_version_str"
  curlVersionStr :: IO CString

foreign import ccall "curl/easy.h curl_global_init"
  curlGlobalInitPrim :: CInt -> IO CInt

foreign import ccall "curl/easy.h curl_global_cleanup"
  curlGlobalCleanup :: IO ()

foreign import ccall "curl/easy.h curl_easy_init"
  easyInitialize :: IO CurlHandle

foreign import ccall "curl/easy.h curl_easy_perform"
  easyPerformPrim :: CurlHandle -> IO CInt

foreign import ccall "curl_easy_duphandle"
  easyDuphandle :: CurlHandle -> IO CurlHandle

foreign import ccall "curl_easy_reset"
  easyReset :: CurlHandle -> IO ()

foreign import ccall "curl_easy_setopt_long"
  easySetoptLong :: CurlHandle -> Int -> Word32 -> IO CInt

foreign import ccall "curl_easy_setopt_longlong"
  easySetoptLLong :: CurlHandle -> Int -> Word64 -> IO CInt

foreign import ccall "curl_easy_setopt_string"
  easySetoptString :: CurlHandle -> Int -> Ptr CChar -> IO CInt

foreign import ccall "curl_easy_setopt_ptr"
  easySetoptPtr :: CurlHandle -> Int -> Ptr a -> IO CInt

foreign import ccall "curl_easy_setopt_ptr"
  easySetoptFptr :: CurlHandle -> Int -> FunPtr a -> IO CInt

foreign import ccall "curl_easy_setopt_ptr"
  easySetoptWfun :: CurlHandle -> Int -> FunPtr WriteFunction -> IO CInt

foreign import ccall "curl_easy_setopt_ptr"
  easySetoptRfun :: CurlHandle -> Int -> FunPtr ReadFunctionPrim -> IO CInt

foreign import ccall "wrapper"
  mkWriter :: WriteFunction -> IO (FunPtr WriteFunction)

foreign import ccall "wrapper"
  mkReader :: ReadFunctionPrim -> IO (FunPtr ReadFunctionPrim)

foreign import ccall "wrapper"
  mkProgress :: ProgressFunction -> IO (FunPtr ProgressFunction)

foreign import ccall "wrapper"
  mkDebugFun :: DebugFunctionPrim -> IO (FunPtr DebugFunctionPrim)

foreign import ccall "wrapper"
  mkSslCtxtFun :: SslCtxtFunction -> IO (FunPtr SslCtxtFunction)