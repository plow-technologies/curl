{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Curl.Internal.Types where

import Control.Concurrent (MVar, newMVar, withMVar)
import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException))
import Control.Monad.Catch (MonadThrow (throwM))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.CaseInsensitive as CaseInsensitive
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Foreign.C (CInt, CString)
import Foreign.Concurrent (addForeignPtrFinalizer)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Network.HTTP.Types (Header, SimpleQueryItem)
import qualified URI.ByteString

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
  deriving anyclass (NFData)

data Curl = Curl
  { handle :: MVar (ForeignPtr CurlPrim), -- libcurl is not thread-safe.
    cleanup :: IORef OptionMap -- deallocate Haskell curl data
  }
  deriving stock (Generic)

-- | Opaque type for libcurl operations across the FFI
data CurlPrim

-- | Pointer to a Curl instance
type CurlHandle = Ptr CurlPrim

-- | A URL, which must be provided for all Curl operations. Will be used to set
-- the @CURLOPT_URL@ option
newtype Url = Url URI.ByteString.URI
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

-- | Construct a 'Url' directly from a 'ByteString'. A @URIParseError@ will result
-- if the URL string is malformed
mkUrl :: MonadThrow m => ByteString -> m Url
mkUrl =
  fmap Url
    . either (throwM . InvalidUrl . show) pure
    . URI.ByteString.parseURI URI.ByteString.laxURIParserOptions

-- | Convenience pattern for directly constructing a 'Url'
-- NOTE: Does not support fragments, and query items require parameters
-- (to maintain straightforward compatibility with @uri-bytestring@)
pattern MkUrl ::
  Scheme -> ByteString -> [ByteString] -> Port -> [SimpleQueryItem] -> Url
pattern MkUrl {scheme, host, path, port, query} <-
  Url
    ( URI.ByteString.URI
        (URI.ByteString.Scheme (readScheme -> scheme))
        ( Just
            ( URI.ByteString.Authority
                Nothing
                (URI.ByteString.Host host)
                (Just (URI.ByteString.Port (fromIntegral -> port)))
              )
          )
        (splitPathSegments -> path)
        (URI.ByteString.Query query)
        Nothing
      )
  where
    MkUrl scheme host path port query =
      Url $
        URI.ByteString.URI
          (URI.ByteString.Scheme (renderScheme scheme))
          (Just authority)
          (renderPathSegments path)
          (URI.ByteString.Query query)
          Nothing
      where
        authority :: URI.ByteString.Authority
        authority =
          URI.ByteString.Authority Nothing (URI.ByteString.Host host)
            . Just
            . URI.ByteString.Port
            $ fromIntegral port

-- | URL scheme. Only includes very common options; for other schemes, use the
-- 'OtherScheme' constructor
data Scheme
  = Http
  | Https
  | File
  | Ftp
  | Ssh
  | OtherScheme ByteString
  deriving stock (Show, Eq, Generic)

type Port = Word32

data Slist

pattern CExitSuccess :: CInt
pattern CExitSuccess = 0

data CurlOtherError
  = InvalidResponse String
  | UnexpectedResponse InfoValue
  | CouldntOpenFile FilePath
  | FlushErrno Int
  | InvalidUrl String
  deriving stock (Show, Generic)

instance Exception CurlOtherError where
  displayException = \case
    InvalidResponse s -> "Invalid response value: " <> s
    UnexpectedResponse iv -> "Unexpected response value " <> show iv
    CouldntOpenFile fp -> "File couldn't be opened: " <> fp
    FlushErrno i -> "fflush failed with " <> show i
    InvalidUrl e -> "Invalid URL: " <> e

data CurlCode
  = Ok
  | UnspportedProtocol
  | FailedInit
  | UrlMalformat
  | NotBuiltIn
  | CouldntResolveProxy
  | CouldntResolveHost
  | CouldntConnect
  | WeirdServerReply
  | RemoteAccessDenied
  | FtpAcceptFailed
  | FtpWeirdPassReply
  | FtpAcceptTimeout
  | FtpWeirdPasvReply
  | FtpWeird227Format
  | FtpCantGetHost
  | Http2Error
  | FtpCouldnSetType
  | PartialFile
  | FtpCouldntRetrFile
  | Obsolete20
  | QuoteError
  | HttpReturnedError
  | WriteError
  | Obsolete24
  | UploadFailed
  | ReadError
  | OutOfMemory
  | OperationTimedOut
  | Obsolete29
  | FtpPortFailed
  | FtpCouldntUseRest
  | Obsolete32
  | RangeError
  | HttpPostError
  | SslConnectError
  | BadDownloadResume
  | FileCouldntReadFile
  | LdapCannotBind
  | LdpapSearchFailed
  | Obsolete40
  | FunctionNotFound
  | AbortedByCallback
  | BadFunctionArgument
  | Obsolete44
  | InterfaceFailed
  | Obsolete46
  | TooManyRedirects
  | UnknownOption
  | SetoptOptionSyntax
  | Obsolete50
  | Obsolete51
  | GotNothing
  | SslEngineNotFound
  | SslEngineSetFailed
  | SendError
  | RecvError
  | Obsolete57
  | SslCertProblem
  | SslCipher
  | SslPeerFailedVerification
  | BadContentEncoding
  | Obsolete62
  | FilesizeExceeded
  | UseSslFailed
  | SendFailRewind
  | SslEngineInitFailed
  | LoginDenied
  | TftpNotFound
  | TftpPerm
  | RemoteDiskFull
  | TftpIllegal
  | TftpUnknownId
  | RemoteFileExists
  | TftpNoSuchUser
  | Obsolete75
  | Obsolete76
  | SslCaCertBadFile
  | RemoveFileNotFound
  | SshError
  | SslShutdownFailed
  | TryAgain
  | SslCrlBadFile
  | SslIssuerError
  | FtpPretFailed
  | RtspCseqError
  | RtspSessionError
  | FtpBadFileList
  | ChunkFailed
  | NoConnectionAvailable
  | SslPinnedPubKeyNotMatch
  | SslInvalidCertStatus
  | Http2StreamError
  | RecursiveApiCall
  | AuthError
  | Http3Error
  | QuicConnectError
  | ProxyError
  | SslClientCert
  | UnrecoverablePoll
  deriving stock (Show, Eq, Generic, Enum)
  deriving anyclass (Exception, NFData)

-- | Causes any non-'CurlOK' 'CurlCode' to be thrown as an exception
withCheckCurlCode :: IO CurlCode -> IO CurlCode
withCheckCurlCode m =
  m >>= \case
    rc@Ok -> pure rc
    rc -> throwM rc

codeFromCInt :: CInt -> CurlCode
codeFromCInt = toEnum . fromIntegral

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
  deriving anyclass (NFData)

data InfoValue
  = String String
  | Long Word32
  | Double Double
  | List [String]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

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

-- Other helpers

splitPathSegments :: ByteString -> [ByteString]
splitPathSegments =
  filter (not . ByteString.Char8.null)
    . ByteString.Char8.split '/'

renderPathSegments :: [ByteString] -> ByteString
renderPathSegments = ("/" <>) . ByteString.Char8.intercalate "/"

readScheme :: ByteString -> Scheme
readScheme = \case
  "http" -> Http
  "https" -> Https
  "file" -> File
  "ftp" -> Ftp
  "ssh" -> Ssh
  b -> OtherScheme b

renderScheme :: Scheme -> ByteString
renderScheme = \case
  Http -> "http"
  Https -> "https"
  File -> "file"
  Ftp -> "ftp"
  Ssh -> "ssh"
  OtherScheme b -> b

renderHeader :: Header -> ByteString
renderHeader (name, v) = CaseInsensitive.original name <> ": " <> v

foreign import ccall "curl/curl.h curl_easy_cleanup" easyCleanup :: CurlHandle -> IO ()

foreign import ccall "curl_slist_free_all" slistFree :: Ptr Slist -> IO ()

foreign import ccall "curl_slist_append"
  slistAppend :: Ptr Slist -> CString -> IO (Ptr Slist)
