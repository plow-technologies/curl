{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

--------------------------------------------------------------------

--------------------------------------------------------------------

-- |
-- Module    : Network.Curl.Types
-- Copyright : (c) Galois Inc 2007-2009
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Basic set of types for the Haskell curl binding, including the
-- @Curl@ handle type which holds the C library stateful connection
-- handle along with a set of cleanup actions tht should be performed
-- upon shutting down the curl session.
module Network.Curl.Types
  ( CurlHandle,
    UrlString,
    Port,
    Slist,
    Curl,
    CurlCode (..),
    curlPrim,
    mkCurl,
    mkCurlWithCleanup,
    OptionMap,
    shareCleanup,
    runCleanup,
    updateCleanup,
    codeFromCInt,
  )
where

import Control.Concurrent
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Word
import Foreign.C (CInt)
import Foreign.Concurrent (addForeignPtrFinalizer)
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Generics (Generic)

data Curl = Curl
  { handle :: MVar (ForeignPtr CurlPrim), -- libcurl is not thread-safe.
    cleanup :: IORef OptionMap -- deallocate Haskell curl data
  }
  deriving stock (Generic)

data CurlPrim

type CurlHandle = Ptr CurlPrim

type UrlString = String

type Port = Word32

data Slist

data CurlCode
  = CurlOK
  | CurlUnspportedProtocol
  | CurlFailedInit
  | CurlUrlMalformat
  | CurlUrlMalformatUser
  | CurlCouldntResolveProxy
  | CurlCouldntResolveHost
  | CurlCouldntConnect
  | CurlFtpWeirdServerReply
  | CurlFtpAccessDenied
  | CurlFtpUserPasswordIncorrect
  | CurlFtpWeirdPassReply
  | CurlFtpWeirdUserReply
  | CurlFtpWeirdPASVReply
  | CurlFtpWeird227Format
  | CurlFtpCantGetHost
  | CurlFtpCantReconnect
  | CurlFtpCouldnSetBinary
  | CurlPartialFile
  | CurlFtpCouldntRetrFile
  | CurlFtpWriteError
  | CurlFtpQuoteError
  | CurlHttpReturnedError
  | CurlWriteError
  | CurlMalformatError
  | CurlFtpCouldnStorFile
  | CurlReadError
  | CurlOutOfMemory
  | CurlOperationTimeout
  | CurlFtpCouldntSetAscii
  | CurlFtpPortFailed
  | CurlFtpCouldntUseRest
  | CurlFtpCouldntGetSize
  | CurlHttpRangeError
  | CurlHttpPostError
  | CurlSslConnectError
  | CurlBadDownloadResume
  | CurlFileCouldntReadFile
  | CurlLdapCannotBind
  | CurlLdpapSearchFailed
  | CurlLibraryNotFound
  | CurlFunctionNotFound
  | CurlAbortedByCallback
  | CurlBadFunctionArgument
  | CurlBadCallingOrder
  | CurlInterfaceFailed
  | CurlBadPasswordEntered
  | CurlTooManyRedirects
  | CurlUnknownTelnetOption
  | CurlTelnetOptionSyntax
  | CurlObsolete
  | CurlSslPeerCertificate
  | CurlGotNothing
  | CurlSslEngineNotFound
  | CurlSslEngineSetFailed
  | CurlSendError
  | CurlRecvError
  | CurlShareInUse
  | CurlSslCertProblem
  | CurlSslCipher
  | CurlSslCACert
  | CurlBadContentEncoding
  | CurlLdapInvalidUrl
  | CurlFilesizeExceeded
  | CurlFtpSslFailed
  | CurlSendFailRewind
  | CurlSslEngineInitFailed
  | CurlLoginDenied
  | CurlTFtpNotFound
  | CurlTFtpPerm
  | CurlTFtpDiskFull
  | CurlTFtpIllegal
  | CurlTFtpUnknownId
  | CurlTFtpExists
  | CurlTFtpNoSuchUser
  | CurlConvFailed
  | CurlConvReqd
  | CurlSslCACertBadFile
  | CurlRemoveFileNotFound
  | CurlSsh
  | CurlSslShutdownFailed
  | CurlAgain
  | CurlSslCRLBadFile
  | CurlSslIssuerError
  deriving stock (Eq, Show, Enum)

codeFromCInt :: CInt -> CurlCode
codeFromCInt x = toEnum (fromIntegral x)

-- | Execute a "primitive" curl operation.
-- NOTE: See warnings about the use of 'withForeignPtr'.
curlPrim :: Curl -> (IORef OptionMap -> CurlHandle -> IO a) -> IO a
curlPrim Curl {handle, cleanup} f =
  withMVar handle $ \h -> withForeignPtr h . f $ cleanup

-- | Allocates a Haskell handle from a C handle.
mkCurl :: CurlHandle -> IO Curl
mkCurl h = mkCurlWithCleanup h emptyOptionMap

-- | Allocates a Haskell handle from a C handle.
mkCurlWithCleanup :: CurlHandle -> OptionMap -> IO Curl
mkCurlWithCleanup h m = do
  cleanup <- newIORef m
  fptr <- newForeignPtr_ h
  handle <- newMVar fptr
  Foreign.Concurrent.addForeignPtrFinalizer fptr $
    easyCleanup h *> runCleanup cleanup
  pure Curl {handle, cleanup}

-- Admin code for cleaning up marshalled data.
-- Note that these functions assume that they are running atomically,
-- so access to them should be protected by a lock.
--------------------------------------------------------------------------------
runCleanup :: IORef OptionMap -> IO ()
runCleanup r = do
  cleanupOptionMap =<< readIORef r
  writeIORef r emptyOptionMap

shareCleanup :: IORef OptionMap -> IO OptionMap
shareCleanup r = do
  old <- readIORef r
  new <- dupOptionMap old
  writeIORef r new
  pure new

updateCleanup :: IORef OptionMap -> Int -> IO () -> IO ()
updateCleanup r option act =
  writeIORef r
    =<< setOptionMap option act
    =<< readIORef r

-- Maps that associate curl options with IO actions to
-- perform cleanup for them.
--------------------------------------------------------------------------------
type OptionMap = IntMap (IO ())

-- | An empty option map.
emptyOptionMap :: OptionMap
emptyOptionMap = IntMap.empty

-- | Set the IO action for an option,
-- executing the previvous action, if there was one.
setOptionMap :: Int -> IO () -> OptionMap -> IO OptionMap
setOptionMap opt new opts = do
  fromMaybe (pure ()) old
  pure m
  where
    old :: Maybe (IO ())
    m :: IntMap (IO ())
    (old, m) = IntMap.insertLookupWithKey (\_ a _ -> a) opt new opts

-- | Execute all IO actions in the map.
cleanupOptionMap :: OptionMap -> IO ()
cleanupOptionMap = sequence_ . IntMap.elems

-- | Replace the actions in a map, with actions that
-- will only be executed the second time they are invoked.
dupOptionMap :: OptionMap -> IO OptionMap
dupOptionMap = fmap IntMap.fromList . traverse dup . IntMap.assocs
  where
    dup :: (a, IO ()) -> IO (a, IO ())
    dup (x, old) = (x,) <$> shareIO old

-- Share a cleanup action.  When we share cleanup duty between two handles
-- we need to ensure that the first handle to perform the cleanup will do
-- nothing (because the other handle still needs the resources).
shareIO :: IO () -> IO (IO ())
shareIO act = do
  v <- newMVar False
  pure $
    takeMVar v >>= \case
      True -> act
      False -> putMVar v True

--------------------------------------------------------------------------------

{- UNUSED:
-- FFI for inalizers.

-- | Make a finalizer from an IO action.
mkIOfin :: IO a -> IO (FinalizerPtr b)
mkIOfin m = mfix (\ptr -> ioFinalizer (m >> freeHaskellFunPtr ptr))

foreign import ccall "wrapper"
  ioFinalizer :: IO () -> IO (FinalizerPtr a)

-}

foreign import capi "curl/curl.h curl_easy_cleanup" easyCleanup :: CurlHandle -> IO ()
