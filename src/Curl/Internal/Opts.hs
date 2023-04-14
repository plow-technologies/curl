module Curl.Internal.Opts where

import Curl.Internal.Post (HttpPost)
import Curl.Internal.Types (Curl, CurlHandle, Port, UrlString)
import Data.Bits (Bits (complement, (.|.)))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CChar, CInt)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Generics (Generic)
import Network.HTTP.Types (ByteRange, renderByteRanges)

pattern GET :: [CurlOption]
pattern GET = [Post False, NoBody False]

pattern POST :: [CurlOption]
pattern POST = [Post True, NoBody False]

pattern HEAD :: [CurlOption]
pattern HEAD = [Post False, NoBody True]

data CurlOption
  = -- | external pointer to pass to as 'WriteFunction's last argument.
    FileObj (Ptr ())
  | -- | the URL to use for next request; can be the full URL or just the authority\/hostname.
    Url UrlString
  | -- | what port to use.
    Port Word32
  | -- | name of proxy
    Proxy String
  | -- | the "user:pass" string to use
    UserPwd String
  | -- | same thing, but for the proxy.
    ProxyUserPwd String
  | -- | byte range to fetch
    Range [ByteRange]
  | -- | external pointer to pass to as 'WriteFunction's last argument.
    -- FIXME This needs to be a pointer to a file
    WriteData FilePath
  | -- | buffer for curl to deposit error messages (must at least CURL_ERROR_SIZE bytes long). Uses standard error if not specified.
    ErrorBuffer (Ptr CChar)
  | -- | callback to handle incoming data.
    WriteFun WriteFunction
  | -- | callback for supplying outgoing\/uploaded data.
    ReadFun ReadFunction
  | -- | number of seconds before timing out curl operation\/request.
    Timeout Word32
  | -- | expected size of uploaded data.
    InFileSize Word32
  | -- | (Multipart) POST data.
    PostFields [String]
  | -- | Set the Referer: header to the given string.
    Referer String
  | -- | The string to feed to the FTP PORT command.
    FtpPort String
  | -- | Set the User-Agent: header to the given string.
    UserAgent String
  | -- | If the bytes per sec drops below the given value, the operation is aborted.
    LowSpeed Word32
  | -- | Upper bound for request to complete.
    LowSpeedTime Word32
  | -- | Byte offset at which the transfer (HTTP or FTP) should start from.
    ResumeFrom Word32
  | -- | Set the Cookie: header to the given cookie (name=value pairs, semicolon-separated) string.
    Cookie String
  | -- | Embellish the outgoing request with the given list of (formatted) header values.
    HttpHeaders [String]
  | -- | (Multipart) POST data.
    Multipart [HttpPost]
  | -- | file holding your private Ssl certificates (default format is PEM).
    SslCert FilePath
  | -- | password to the above file.
    SslPassword String
  | -- | an alias for the previous.
    SslKeyPassword String
  | -- | If true, convert Unix newlines into CrlFs when transferring.
    Crlf Bool
  | -- | Sequence of FTP commands to execute prior to the main request.
    Quote [String]
  | -- | State \/ pointer argument to pass to WriteFunction callback.
    WriteHeader (Ptr ())
  | -- | Path to file holding initial cookie data; also enables cookie handling.
    CookieFile FilePath
  | -- | What protocol to attempt using (0:default;1:TLS;2:Sslv2;3:Sslv3)
    SslVersion Word32
  | -- | How to interpret a conditional time value.
    TimeCondition TimeCond
  | -- | Number of secs since Jan 1, 1970. Interpretation is determined by TimeCondition.
    TimeValue Word32
  | -- | String holding alternative request command (WebDAV anyone?)
    CustomRequest String
  | -- | List of commands to issue to FTP server after the main request.
    PostQuote [String]
  | -- | Not sure what this one does; something about passing it to the output function.
    WriteInfo String
  | -- | Control verbosity
    Verbose Bool
  | -- | Display outgoing and incoming headers
    Header Bool
  | -- | Control progress meter
    NoProgress Bool
  | -- | Use HEAD instead of GET
    NoBody Bool
  | -- | If status response is >= 300, return an error (and no other output).
    FailOnError Bool
  | -- | Control the main dataflow, i.e., True to perform uploads.
    Upload Bool
  | -- | Issue a POST request.
    Post Bool
  | -- | Switch NLST for FTP directory listings
    FtpListOnly Bool
  | -- | Control if FTP uploads append rather than overwrite files
    FtpAppend Bool
  | -- | control how or if a user's.netrc will be consulted for user:password
    UseNetRc NetRcOption
  | -- | Handle auto-redirects by chasing down Location: values in responses.
    FollowLocation Bool
  | -- | Turn on ASCII transfers for FTP transfers; default is binary (i.e. off).
    TransferTextASCII Bool
  | -- | Use PUT to upload data.
    Put Bool
  | -- | callback for showing progress
    ProgressFun ProgressFunction
  | -- | state argumentto pass to progress callback.
    ProgressData (Ptr ())
  | -- | Control if the Referer: field is set upon following Location: redirects
    AutoReferer Bool
  | -- | (Numeric) proxy port to use.
    ProxyPort Word32
  | -- | Size of the POSTed data.
    PostFieldSize Word32
  | -- | tunnel all HTTP operations through the proxy.
    HttpProxyTunnel Bool
  | -- | Interface name of outgoing network interface ( network interface, IP address, host name.)
    Interface String
  | -- | Kerberos security level ("clear", "safe", "confidential", "private" are good values, seemingly.)
    Krb4Level String
  | -- | Enable the authentication of peer certificate. Default is True.
    SslVerifyPeer Bool
  | -- | If verifying peer's certificate, use certificates in this file to do so.
    CaInfo FilePath
  | -- | Maximum number of Location: redirects to chase down before giving up.
    MaxRedirs Word32
  | -- | Try to determine the modification date of remote document; can be queried for.
    Filetime Bool
  | -- | List of commands to use for initial telnet negotiations.
    TelnetOptions [String]
  | -- | Maximum number of cached active connections.
    MaxConnects Word32
  | -- | No effect (obsolete.)
    ClosePolicy Word32
  | -- | Force the opening up a new connection rather than try to reuse active connections. Default is not to.
    FreshConnect Bool
  | -- | Do not reuse the connection of next transfer when done.
    ForbidReuse Bool
  | -- | Path to file used to seed (Open)Ssl PRNG.
    RandomFile FilePath
  | -- | Path to domain socket of EG Daemon.
    EgdSocket FilePath
  | -- | max number of seconds to wait for the initial connection to happen.
    ConnectTimeout Word32
  | -- | callback used to handle _incoming_ header data.
    HeadFun WriteFunction
  | -- | Revert to a GET for the next request.
    Get Bool
  | -- | Perform Common name checking in peer certificate (1=> existence;2=> matches hostname.)
    SslVerifyHost Word32
  | -- | Path to file where additional cookie information will be stored.
    CookieJar FilePath
  | -- | Colon-separated string list of cipher preferences to use for upcoming connection (e.g., "3DES:+RSA")
    SslCipherList String
  | -- | What HTTP version to use, should you want to drop back for some reason.
    HttpVersion HttpVersion
  | -- | Attempt the use of EPSV before PASV for passive FTP downloads.
    FtpUseEPSV Bool
  | -- | The format of your certificates ("PEM", "DER")
    SslCertType String
  | -- | Filename of private key.
    SslKey FilePath
  | -- | Format of private key; use "ENG" to load from a crypto engine.
    SslKeyType String
  | -- | Name of crypto engine to use.
    SslEngine String
  | -- | Make crypto engine the default for crypto operations.
    SslEngineDefault
  | -- | Have library uses its MT-unfriendly Dns global cache.
    DnsUseGlobalCache Bool
  | -- | Number of seconds to cache results of Dns lookups in memory.
    DnsCacheTimeout Word32
  | -- | FTP commands to issue after connection and transfer mode has been set.
    PreQuote [String]
  | -- | callback to catch and report transfer operations.
    DebugFun DebugFunction
  | -- | state argument to pass to debug callback.
    DebugData (Ptr ())
  | -- | Signal the start of a cookie session, ignoring previous session cookies.
    CookieSession Bool
  | -- | Directory holding Ca certificates; used when verifying peer certificate.
    CaPath FilePath
  | -- | Turn (down, presumably) the buffers the received data is chunked up into (and reported to the WriteFunction.) A hint, library is free to ignore.
    BufferSize Word32
  | -- | Turn off use of signals internally.
    NoSignal Bool
  | -- | Share handles are used for sharing data among concurrent  objects.
    Share (Ptr ())
  | -- | What type of proxy to use.
    ProxyType Word32
  | -- | What to report in the Accept-Encoding: header
    Encoding String
  | -- | Data associated with a  handle.
    Private (Ptr ())
  | -- | Alternatives to standard 200 OK response strings; whatever it takes, I suppose.
    Http200Aliases String
  | -- | Pass on user:pass when following redirects.
    UnrestrictedAuth Bool
  | -- | For active FTP downloads, try using EPRT command over LPRT.
    FtppUseEPRT Bool
  | -- | State your authentication preferences.
    Auth [HttpAuth]
  | -- | callback to handle setting up Ssl connections; have the power to abort them.
    SslCtxFun SslCtxtFunction
  | -- | state argument to pass into the above callback.
    SslCtxData (Ptr ())
  | -- | Have remote directories be created if not already there
    FtpCreateMissingDirs Bool
  | -- | What preferred authentication schemes to use wrt. proxy.
    ProxyAuth [HttpAuth]
  | -- | max number of seconds to wait for remote server to ACK commands.
    FtpResponseTimeout Word32
  | -- | Whether to resolve wrt IPv4 or IPv6.
    IPResolve Word32
  | -- | Limit the number of bytes you're willing to download.
    MaxFileSize Word32
  | -- | Wider alternative of option giving upper bound of uploaded content (-1 => unknown.)
    InFileSizeLarge Word64
  | -- | Wider alternative for specifying initial transfer offset.
    ResumeFromLarge Word64
  | -- | Wider alternative for specifying max download size.
    MaxFileSizeLarge Word64
  | -- | Path to user\'s .netrc
    NetrcFile FilePath
  | -- | Try enabling the use of Ssl for FTP control connections and\/or transfers.
    FtpSsl Word32
  | -- | Size of data to POST; if unspecified (or -1), curl uses strlen().
    PostFieldSizeLarge Word64
  | -- | Turn on or off the TCP\/IP NODELAY option.
    TcpNoDelay Bool
  | -- | Twiddle if TLS or Ssl is used.
    FtpSslAuth Word32
  | -- | somewhat obscure callback for handling read stream resets.
    IoctlFun (Ptr ())
  | -- | state argument to the above.
    IoctlData (Ptr ())
  | -- | The string to use when server asks for account info.
    FtpAccount String
  | -- | Cookie string to pass cookie engine; "ALL" scrubs all cookie info; "SESS" scrubs session ones.
    CookieList String
  | -- | If Content-Length: values are troublesome (wrong, perhaps?), use this option to ignore using them as guidance.
    IgnoreContentLength Bool
  | -- | Ignore IP address in 227 responses.
    FtpSkipPASVIP Bool
  | -- | How to navigate to a file on the remote server (single, multiple CWDs).
    FtpFileMethod Word32
  | -- | What local port to use for established connection.
    LocalPort Port
  | -- | Number of attempts at finding local ports (using LocalPort as initial base.)
    LocalPortRange Port
  | -- | If enabled, perform all steps up until actual transfer.
    ConnectOnly Bool
  | -- | callback for doing character translations from network format.
    ConvFromNetworkFun (Ptr ())
  | -- | callback for doing character translations to network format.
    ConvToNetworkFun (Ptr ())
  | -- | callback for translating UTF8 into host encoding.
    ConvFromUtf8Fun (Ptr ())
  | -- | Specifies throttle value for outgoing data.
    MaxSendSpeedLarge Word64
  | -- | Specifies throttle for incoming data.
    MaxRecvSpeedLarge Word64
  | -- | Alternative (to user:pass) for FTP authentication; weird.
    FtpAlternativeToUser String
  | -- | callback that's injected between socket creation and connection.
    SockOptFun (Ptr ())
  | -- | state argument to the above.
    SockOptData (Ptr ())
  | -- | Enable the Ssl session id cache; default is on, so use this to disable.
    SslSessionIdCache Bool
  | -- | Ssh authentication methods to use.
    SshAuthTypes [SshAuthType]
  | -- | Path to file holding user's Ssh public key.
    SshPublicKeyFile FilePath
  | -- | Path to file holding user's Ssh private key.
    SshPrivateKeyFile FilePath
  | -- | Send Ccc command after FTP connection has been authenticated.
    FtpSslCcc Bool
  | -- | Max number of milliseconds that a transfer may take.
    TimeoutMS Word32
  | -- | Max number of milliseconds that a connection attempt may take to complete.
    ConnectTimeoutMS Word32
  | -- | Disable transfer decoding; if disabled, curl will turn off chunking.
    HttpTransferDecoding Bool
  | -- | Disable content decoding, getting the raw bits.
    HttpContentDecoding Bool
  | NewFilePerms Word32
  | NewDirectoryPerms Word32
  | PostRedirect Bool
  | SshHostPublicKeyMD5 String
  | CopyPostFields Bool
  | ProxyTransferMode Word32
  | CrlFile FilePath
  | IssuerCert FilePath
  | AddressScope Word32
  | CertInfo Word32
  | UserName String
  | UserPassword String
  | ProxyUser String
  | ProxyPassword String
  deriving stock (Show)

data HttpVersion
  = NoVersion
  | HttpVersion10
  | HttpVersion11
  deriving stock (Show, Eq, Generic, Ord, Enum)

data TimeCond
  = NoTimeCond
  | IfModSince
  | IfUnmodSince
  | LastMode
  deriving stock (Show, Eq, Generic, Ord, Enum)

data NetRcOption
  = NetRcIgnored
  | NetRcOptional
  | NetRcRequired
  deriving stock (Show, Eq, Generic, Ord, Enum)

data HttpAuth
  = HttpAuthNone
  | HttpAuthBasic
  | HttpAuthDigest
  | HttpAuthGSSNegotiate
  | HttpAuthNTLM
  | HttpAuthAny
  | HttpAuthAnySafe
  deriving stock (Show, Eq, Generic, Ord, Enum)

toHttpAuthMask :: [HttpAuth] -> Word32
toHttpAuthMask [] = 0
toHttpAuthMask (x : xs) = case x of
  HttpAuthNone -> vs
  HttpAuthBasic -> 0x1 .|. vs
  HttpAuthDigest -> 0x2 .|. vs
  HttpAuthGSSNegotiate -> 0x4 .|. vs
  HttpAuthNTLM -> 0x8 .|. vs
  HttpAuthAny -> complement 0 .|. vs
  HttpAuthAnySafe -> complement 1 .|. vs
  where
    vs :: Word32
    vs = toHttpAuthMask xs

data SshAuthType
  = SshAuthAny
  | SshAuthNone
  | SshAuthPublickey
  | SshAuthPassword
  | SshAuthHost
  | SshAuthKeyboard
  deriving stock (Show)

toSshAuthMask :: [SshAuthType] -> Word32
toSshAuthMask [] = 0
toSshAuthMask (x : xs) = case x of
  SshAuthAny -> complement 0 .|. vs
  SshAuthNone -> vs
  SshAuthPublickey -> 1 .|. vs
  SshAuthPassword -> 2 .|. vs
  SshAuthHost -> 4 .|. vs
  SshAuthKeyboard -> 8 .|. vs
  where
    vs :: Word32
    vs = toSshAuthMask xs

newtype WriteFunction
  = WriteFunction
      ( Ptr CChar -> --  pointer to external buffer holding data
        CInt -> --  width (in bytes) of each item
        CInt -> --  number of items
        Ptr () -> --  state argument (file pointer etc.)
        IO CInt --  number of bytes written.
      )
  deriving (Show) via (ShowFun WriteFunction)

newtype ReadFunction
  = ReadFunction
      ( Ptr CChar -> --  pointer to external buffer to fill in.
        CInt -> --  width (in bytes) of each item
        CInt -> --  number of items
        Ptr () -> --  state argument (file pointer etc.)
        IO (Maybe CInt) --  how many bytes was copied into buffer; Nothing => abort.
      )
  deriving (Show) via (ShowFun ReadFunction)

type ReadFunctionPrim =
  Ptr CChar ->
  CInt ->
  CInt ->
  Ptr () ->
  IO CInt

newtype ProgressFunction
  = ProgressFunction
      ( Ptr () -> --  state argument
        Double -> --  expected download totals
        Double -> --  download totals so far
        Double -> --  expected upload totals
        Double -> --  upload totals so far
        IO CInt --  not sure; 0 is a good one.
      )
  deriving (Show) via (ShowFun ProgressFunction)

newtype DebugFunction
  = DebugFunction
      ( Curl -> --  connection handle
        DebugInfo -> --  type of call
        Ptr CChar -> --  data buffer
        CInt -> --  length of buffer
        Ptr () -> --  state argument
        IO () --  always 0
      )
  deriving (Show) via (ShowFun DebugFunction)

type DebugFunctionPrim =
  CurlHandle -> --  connection handle
  CInt -> --  type of call
  Ptr CChar -> --  data buffer
  CInt -> --  length of buffer
  Ptr () -> --  state argument
  IO CInt --  always 0

newtype SslCtxtFunction
  = SslCtxtFunction
      ( CurlHandle -> --  connection handle
        Ptr () -> --  the Ssl_CTX handle
        Ptr () -> --  state argument
        IO CInt
      )
  deriving (Show) via (ShowFun SslCtxtFunction)

newtype ShowFun a = ShowFun a

instance Show (ShowFun a) where
  show _ = "<fun>"

data DebugInfo
  = InfoText
  | InfoHeaderIn
  | InfoHeaderOut
  | InfoDataIn
  | InfoDataOut
  | InfoSslDataIn
  | InfoSslDataOut
  deriving stock (Show, Eq, Generic, Ord, Enum)

curlReadfuncAbort :: CInt
curlReadfuncAbort = 0x10000000

baseLong :: Int
baseLong = 0

baseObject :: Int
baseObject = 10000

baseFunction :: Int
baseFunction = 20000

baseOffT :: Int
baseOffT = 30000

unmarshallOption :: Unmarshaller a -> CurlOption -> IO a
unmarshallOption um@Unmarshaller {..} = \case
  FileObj x -> pointer (withObject 1) x
  Url x -> string (withObject 2) x
  Port x -> long (withLong 3) x
  Proxy x -> string (withObject 4) x
  UserPwd x -> string (withObject 5) x
  ProxyUserPwd x -> string (withObject 6) x
  Range x ->
    string (withObject 7)
      . ByteString.Char8.unpack
      . fromMaybe byteRanges
      -- `renderByteRanges` will add the `bytes=` prefix, but this won't be
      -- accepted by libcurl. It's still nicer to use the `ByteRanges` type
      -- and just strip the suffix
      . ByteString.Char8.stripPrefix "bytes="
      $ byteRanges
    where
      byteRanges :: ByteString
      byteRanges = renderByteRanges x
  WriteData x -> string (withObject 9) x
  ErrorBuffer x -> unmarshalCptr um (withObject 10) x
  WriteFun x -> writeFun (withFunc 11) x
  ReadFun x -> readFun (withFunc 12) x
  Timeout x -> long (withLong 13) x
  InFileSize x -> long (withLong 14) x
  PostFields x -> string (withObject 15) $ intercalate "&" x
  Referer x -> string (withObject 16) x
  FtpPort x -> string (withObject 17) x
  UserAgent x -> string (withObject 18) x
  LowSpeed x -> long (withLong 19) x
  LowSpeedTime x -> long (withLong 20) x
  ResumeFrom x -> long (withLong 21) x
  Cookie x -> string (withObject 22) x
  HttpHeaders x -> strings (withObject 23) x
  Multipart x -> posts (withObject 24) x
  SslCert x -> string (withObject 25) x
  SslPassword x -> string (withObject 26) x
  SslKeyPassword x -> string (withObject 26) x -- yes, duplicate.
  Crlf x -> unmarshalBool um (withLong 27) x
  Quote x -> strings (withObject 28) x
  WriteHeader x -> pointer (withObject 29) x
  CookieFile x -> string (withObject 31) x
  SslVersion x -> long (withLong 32) x
  TimeCondition x -> unmarshalEnum um (withLong 33) x
  TimeValue x -> long (withLong 34) x
  CustomRequest x -> string (withObject 36) x
  PostQuote x -> strings (withObject 39) x
  WriteInfo x -> string (withObject 40) x
  Verbose x -> unmarshalBool um (withLong 41) x
  Header x -> unmarshalBool um (withLong 42) x
  NoProgress x -> unmarshalBool um (withLong 43) x
  NoBody x -> unmarshalBool um (withLong 44) x
  FailOnError x -> unmarshalBool um (withLong 45) x
  Upload x -> unmarshalBool um (withLong 46) x
  Post x -> unmarshalBool um (withLong 47) x
  FtpListOnly x -> unmarshalBool um (withLong 48) x
  FtpAppend x -> unmarshalBool um (withLong 50) x
  UseNetRc x -> unmarshalEnum um (withLong 51) x
  FollowLocation x -> unmarshalBool um (withLong 52) x
  TransferTextASCII x -> unmarshalBool um (withLong 53) x
  Put x -> unmarshalBool um (withLong 54) x
  ProgressFun x -> progressFun (withFunc 56) x
  ProgressData x -> pointer (withObject 57) x
  AutoReferer x -> unmarshalBool um (withLong 58) x
  ProxyPort x -> long (withLong 59) x
  PostFieldSize x -> long (withLong 60) x
  HttpProxyTunnel x -> unmarshalBool um (withLong 61) x
  Interface x -> string (withObject 62) x
  Krb4Level x -> string (withObject 63) x
  SslVerifyPeer x -> unmarshalBool um (withLong 64) x
  CaInfo x -> string (withObject 65) x
  MaxRedirs x -> long (withLong 68) x
  Filetime x -> unmarshalBool um (withLong 69) x
  TelnetOptions x -> strings (withObject 70) x
  MaxConnects x -> long (withLong 71) x
  ClosePolicy x -> long (withLong 72) x
  FreshConnect x -> unmarshalBool um (withLong 74) x
  ForbidReuse x -> unmarshalBool um (withLong 75) x
  RandomFile x -> string (withObject 76) x
  EgdSocket x -> string (withObject 77) x
  ConnectTimeout x -> long (withLong 78) x
  HeadFun x -> writeFun (withFunc 79) x
  Get x -> unmarshalBool um (withLong 80) x
  SslVerifyHost x -> long (withLong 81) x
  CookieJar x -> string (withObject 82) x
  SslCipherList x -> string (withObject 83) x -- a string (withObjectr a withLong-list of them)?
  HttpVersion x -> unmarshalEnum um (withLong 84) x
  FtpUseEPSV x -> unmarshalBool um (withLong 85) x
  SslCertType x -> string (withObject 86) x
  SslKey x -> string (withObject 87) x
  SslKeyType x -> string (withObject 88) x
  SslEngine x -> string (withObject 89) x
  SslEngineDefault -> unmarshalBool um (withLong 90) True
  DnsUseGlobalCache x -> unmarshalBool um (withLong 91) x
  DnsCacheTimeout x -> long (withLong 92) x
  PreQuote x -> strings (withObject 93) x
  DebugFun x -> debugFun (withFunc 94) x
  DebugData x -> pointer (withObject 95) x
  CookieSession x -> unmarshalBool um (withLong 96) x
  CaPath x -> string (withObject 97) x
  BufferSize x -> long (withLong 98) x
  NoSignal x -> unmarshalBool um (withLong 99) x
  Share x -> pointer (withObject 100) x
  ProxyType x -> unmarshalEnum um (withLong 101) x
  Encoding x -> string (withObject 102) x
  Private x -> pointer (withObject 103) x
  Http200Aliases x -> string (withObject 104) x -- correct?
  UnrestrictedAuth x -> unmarshalBool um (withLong 105) x
  FtppUseEPRT x -> unmarshalBool um (withLong 106) x
  Auth xs -> long (withLong 107) (toHttpAuthMask xs)
  SslCtxFun x -> sslctxt (withFunc 108) x
  SslCtxData x -> pointer (withObject 109) x
  FtpCreateMissingDirs x -> unmarshalBool um (withLong 110) x
  ProxyAuth x -> long (withLong 111) (toHttpAuthMask x)
  FtpResponseTimeout x -> long (withLong 112) x
  IPResolve x -> long (withLong 113) x
  MaxFileSize x -> long (withLong 114) x
  InFileSizeLarge x -> llong (withOffset 115) x
  ResumeFromLarge x -> llong (withOffset 116) x
  MaxFileSizeLarge x -> llong (withOffset 117) x
  NetrcFile x -> string (withObject 118) x
  FtpSsl x -> unmarshalEnum um (withLong 119) x
  PostFieldSizeLarge x -> llong (withOffset 120) x
  TcpNoDelay x -> unmarshalBool um (withLong 121) x
  FtpSslAuth x -> unmarshalEnum um (withLong 129) x
  IoctlFun x -> ioctlFun (withFunc 130) x
  IoctlData x -> pointer (withObject 131) x
  FtpAccount x -> string (withObject 134) x
  CookieList x -> string (withObject 135) x
  IgnoreContentLength x -> unmarshalBool um (withLong 136) x
  FtpSkipPASVIP x -> unmarshalBool um (withLong 137) x
  FtpFileMethod x -> unmarshalEnum um (withLong 138) x
  LocalPort x -> long (withLong 139) x
  LocalPortRange x -> long (withLong 140) x
  ConnectOnly x -> unmarshalBool um (withLong 141) x
  ConvFromNetworkFun x -> convFromNetwork (withFunc 142) x
  ConvToNetworkFun x -> convToNetwork (withFunc 143) x
  ConvFromUtf8Fun x -> convFromUtf8 (withFunc 144) x
  MaxSendSpeedLarge x -> llong (withOffset 145) x
  MaxRecvSpeedLarge x -> llong (withOffset 146) x
  FtpAlternativeToUser x -> string (withObject 147) x
  SockOptFun x -> sockoptFun (withFunc 148) x
  SockOptData x -> pointer (withObject 149) x
  SslSessionIdCache x -> unmarshalBool um (withLong 150) x
  SshAuthTypes xs -> long (withLong 151) (toSshAuthMask xs)
  SshPublicKeyFile x -> string (withObject 152) x
  SshPrivateKeyFile x -> string (withObject 153) x
  FtpSslCcc x -> unmarshalBool um (withLong 154) x
  TimeoutMS x -> long (withLong 155) x
  ConnectTimeoutMS x -> long (withLong 156) x
  HttpTransferDecoding x -> unmarshalBool um (withLong 157) x
  HttpContentDecoding x -> unmarshalBool um (withLong 158) x
  NewFilePerms x -> long (withLong 159) x
  NewDirectoryPerms x -> long (withLong 160) x
  PostRedirect x -> unmarshalBool um (withLong 161) x
  SshHostPublicKeyMD5 x -> string (withLong 162) x
  CopyPostFields x -> unmarshalBool um (withLong 165) x
  ProxyTransferMode x -> long (withLong 166) x
  CrlFile x -> string (withLong 169) x
  IssuerCert x -> string (withLong 170) x
  AddressScope x -> long (withLong 171) x
  CertInfo x -> long (withLong 172) x
  UserName x -> string (withLong 173) x
  UserPassword x -> string (withLong 174) x
  ProxyUser x -> string (withLong 175) x
  ProxyPassword x -> string (withLong 176) x
  where
    withLong :: Int -> Int
    withLong = (baseLong +)

    withObject :: Int -> Int
    withObject = (baseObject +)

    withFunc :: Int -> Int
    withFunc = (baseFunction +)

    withOffset :: Int -> Int
    withOffset = (baseOffT +)

data Unmarshaller a = Unmarshaller
  { long :: Int -> Word32 -> IO a,
    llong :: Int -> Word64 -> IO a,
    string :: Int -> String -> IO a,
    strings :: Int -> [String] -> IO a,
    pointer :: Int -> Ptr () -> IO a,
    writeFun :: Int -> WriteFunction -> IO a,
    readFun :: Int -> ReadFunction -> IO a,
    progressFun :: Int -> ProgressFunction -> IO a,
    debugFun :: Int -> DebugFunction -> IO a,
    posts :: Int -> [HttpPost] -> IO a,
    sslctxt :: Int -> SslCtxtFunction -> IO a,
    ioctlFun :: Int -> Ptr () -> IO a,
    convFromNetwork :: Int -> Ptr () -> IO a,
    convToNetwork :: Int -> Ptr () -> IO a,
    convFromUtf8 :: Int -> Ptr () -> IO a,
    sockoptFun :: Int -> Ptr () -> IO a
  }

unmarshalBool :: Unmarshaller a -> Int -> Bool -> IO a
unmarshalBool Unmarshaller {long} x =
  long x . \case
    True -> 1
    False -> 0

unmarshalEnum :: Enum b => Unmarshaller a -> Int -> b -> IO a
unmarshalEnum Unmarshaller {long} x = long x . fromIntegral . fromEnum

unmarshalCptr :: Unmarshaller a -> Int -> Ptr CChar -> IO a
unmarshalCptr Unmarshaller {pointer} x = pointer x . castPtr
