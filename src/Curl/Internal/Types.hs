module Curl.Internal.Types where

import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Exception (Exception (displayException))
import Control.Monad.Catch (MonadThrow (throwM))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Foreign.C (CInt)
import Foreign.Concurrent (addForeignPtrFinalizer)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Network.HTTP.Types (Header)

-- | 'CurlResponse' is a record type encoding all the information
-- embodied in a response to your Curl request. Currently only used
-- to gather up the results of doing a GET in 'curlGetResponse'.
data CurlResponse = CurlResponse
  { curlCode :: CurlCode,
    status :: Int,
    statusLine :: ByteString,
    headers :: [Header],
    body :: Lazy.ByteString.ByteString,
    info :: Map Info InfoValue
  }
  deriving stock (Show, Generic)

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

pattern CurlExitSuccess :: CInt
pattern CurlExitSuccess = 0

data CurlOtherError
  = InvalidResponse String
  | UnexpectedResponse InfoValue
  deriving stock (Show, Generic)

instance Exception CurlOtherError where
  displayException = \case
    InvalidResponse s -> "Invalid response value: " <> s
    UnexpectedResponse iv -> "Unexpected response value " <> show iv

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
  deriving anyclass (Exception)

withCheckCurlCode :: IO CurlCode -> IO CurlCode
withCheckCurlCode m =
  m >>= \case
    rc@CurlOK -> pure rc
    rc -> throwM rc

codeFromCInt :: CInt -> CurlCode
codeFromCInt x = toEnum (fromIntegral x)

data Info
  = EffectiveUrl
  | ResponseCode
  | TotalTime
  | NameLookupTime
  | ConnectTime
  | PreTransferTime
  | SizeUpload
  | SizeDownload
  | SpeedDownload
  | SpeedUpload
  | HeaderSize
  | RequestSize
  | SslVerifyResult
  | Filetime
  | ContentLengthDownload
  | ContentLengthUpload
  | StartTransferTime
  | ContentType
  | RedirectTime
  | RedirectCount
  | Private
  | HttpConnectCode
  | HttpAuthAvail
  | ProxyAuthAvail
  | OSErrno
  | NumConnects
  | SslEngines
  | CookieList
  | LastSocket
  | FtpEntryPath
  deriving stock (Show, Eq, Generic, Ord, Enum, Bounded)

data InfoValue
  = String String
  | Long Word32
  | Double Double
  | List [String]
  deriving stock (Show, Eq, Generic)

-- | Execute a "primitive" curl operation.
-- NOTE: See warnings about the use of 'withForeignPtr'.
curlPrim :: Curl -> (IORef OptionMap -> CurlHandle -> IO a) -> IO a
curlPrim Curl {handle, cleanup} f =
  withMVar handle $ \curl -> withForeignPtr curl . f $ cleanup

-- | Allocates a Haskell handle from a C handle.
mkCurl :: CurlHandle -> IO Curl
mkCurl curl = do
  cleanup <- newIORef emptyOptionMap
  fptr <- newForeignPtr_ curl
  handle <- newMVar fptr
  Foreign.Concurrent.addForeignPtrFinalizer fptr $
    easyCleanup curl *> runCleanup cleanup
  pure Curl {handle, cleanup}

-- Admin code for cleaning up marshalled data.
-- Note that these functions assume that they are running atomically,
-- so access to them should be protected by a lock.
--------------------------------------------------------------------------------
runCleanup :: IORef OptionMap -> IO ()
runCleanup r = do
  cleanupOptionMap =<< readIORef r
  writeIORef r emptyOptionMap

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

foreign import ccall "curl/curl.h curl_easy_cleanup" easyCleanup :: CurlHandle -> IO ()
