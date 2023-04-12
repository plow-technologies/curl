--------------------------------------------------------------------

--------------------------------------------------------------------

-- |
-- Module    : Network.Curl.Opts
-- Copyright : (c) Galois Inc 2007-2009
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- This module contains the various options that specify what happens
-- when we use @perform@ on a @Curl@ handle.
module Network.Curl.Opts where

import Data.Bits
import Data.List
import Foreign.C.Types
import Foreign.Ptr
import Network.Curl.Post
import Network.Curl.Types

data CurlOption
  = -- | external pointer to pass to as 'WriteFunction's last argument.
    CurlFileObj (Ptr ())
  | -- | the URL to use for next request; can be the full URL or just the authority\/hostname.
    CurlURL URLString
  | -- | what port to use.
    CurlPort Long
  | -- | name of proxy
    CurlProxy String
  | -- | the "user:pass" string to use
    CurlUserPwd String
  | -- | same thing, but for the proxy.
    CurlProxyUserPwd String
  | -- | byte range to fetch
    CurlRange String
  | -- | external pointer to pass to as 'WriteFunction's last argument.
    CurlInFile FilePath
  | -- | buffer for curl to deposit error messages (must at least CURL_ERROR_SIZE bytes long). Uses standard error if not specified.
    CurlErrorBuffer (Ptr CChar)
  | -- | callback to handle incoming data.
    CurlWriteFunction WriteFunction
  | -- | callback for supplying outgoing\/uploaded data.
    CurlReadFunction ReadFunction
  | -- | number of seconds before timing out curl operation\/request.
    CurlTimeout Long
  | -- | expected size of uploaded data.
    CurlInFileSize Long
  | -- | (Multipart) POST data.
    CurlPostFields [String]
  | -- | Set the Referer: header to the given string.
    CurlReferer String
  | -- | The string to feed to the FTP PORT command.
    CurlFtpPort String
  | -- | Set the User-Agent: header to the given string.
    CurlUserAgent String
  | -- | If the bytes per sec drops below the given value, the operation is aborted.
    CurlLowSpeed Long
  | -- | Upper bound for request to complete.
    CurlLowSpeedTime Long
  | -- | Byte offset at which the transfer (HTTP or FTP) should start from.
    CurlResumeFrom Long
  | -- | Set the Cookie: header to the given cookie (name=value pairs, semicolon-separated) string.
    CurlCookie String
  | -- | Embellish the outgoing request with the given list of (formatted) header values.
    CurlHttpHeaders [String]
  | -- | (Multipart) POST data.
    CurlHttpPost [HttpPost]
  | -- | file holding your private SSL certificates (default format is PEM).
    CurlSSLCert FilePath
  | -- | password to the above file.
    CurlSSLPassword String
  | -- | an alias for the previous.
    CurlSSLKeyPassword String
  | -- | If true, convert Unix newlines into CRLFs when transferring.
    CurlCRLF Bool
  | -- | Sequence of FTP commands to execute prior to the main request.
    CurlQuote [String]
  | -- | State \/ pointer argument to pass to WriteFunction callback.
    CurlWriteHeader (Ptr ())
  | -- | Path to file holding initial cookie data; also enables cookie handling.
    CurlCookieFile FilePath
  | -- | What protocol to attempt using (0:default;1:TLS;2:SSLv2;3:SSLv3)
    CurlSSLVersion Long
  | -- | How to interpret a conditional time value.
    CurlTimeCondition TimeCond
  | -- | Number of secs since Jan 1, 1970. Interpretation is determined by CurlTimeCondition.
    CurlTimeValue Long
  | -- | String holding alternative request command (WebDAV anyone?)
    CurlCustomRequest String
  | -- | List of commands to issue to FTP server after the main request.
    CurlPostQuote [String]
  | -- | Not sure what this one does; something about passing it to the output function.
    CurlWriteInfo String
  | -- | Control verbosity
    CurlVerbose Bool
  | -- | Display outgoing and incoming headers
    CurlHeader Bool
  | -- | Control progress meter
    CurlNoProgress Bool
  | -- | Use HEAD instead of GET
    CurlNoBody Bool
  | -- | If status response is >= 300, return an error (and no other output).
    CurlFailOnError Bool
  | -- | Control the main dataflow, i.e., True to perform uploads.
    CurlUpload Bool
  | -- | Issue a POST request.
    CurlPost Bool
  | -- | Switch NLST for FTP directory listings
    CurlFtpListOnly Bool
  | -- | Control if FTP uploads append rather than overwrite files
    CurlFtpAppend Bool
  | -- | control how or if a user's.netrc will be consulted for user:password
    CurlUseNetRc NetRcOption
  | -- | Handle auto-redirects by chasing down Location: values in responses.
    CurlFollowLocation Bool
  | -- | Turn on ASCII transfers for FTP transfers; default is binary (i.e. off).
    CurlTransferTextASCII Bool
  | -- | Use PUT to upload data.
    CurlPut Bool
  | -- | callback for showing progress
    CurlProgressFunction ProgressFunction
  | -- | state argumentto pass to progress callback.
    CurlProgressData (Ptr ())
  | -- | Control if the Referer: field is set upon following Location: redirects
    CurlAutoReferer Bool
  | -- | (Numeric) proxy port to use.
    CurlProxyPort Long
  | -- | Size of the POSTed data.
    CurlPostFieldSize Long
  | -- | tunnel all HTTP operations through the proxy.
    CurlHttpProxyTunnel Bool
  | -- | Interface name of outgoing network interface ( network interface, IP address, host name.)
    CurlInterface String
  | -- | Kerberos security level ("clear", "safe", "confidential", "private" are good values, seemingly.)
    CurlKrb4Level String
  | -- | Enable the authentication of peer certificate. Default is True.
    CurlSSLVerifyPeer Bool
  | -- | If verifying peer's certificate, use certificates in this file to do so.
    CurlCAInfo FilePath
  | -- | Maximum number of Location: redirects to chase down before giving up.
    CurlMaxRedirs Long
  | -- | Try to determine the modification date of remote document; can be queried for.
    CurlFiletime Bool
  | -- | List of commands to use for initial telnet negotiations.
    CurlTelnetOptions [String]
  | -- | Maximum number of cached active connections.
    CurlMaxConnects Long
  | -- | No effect (obsolete.)
    CurlClosePolicy Long
  | -- | Force the opening up a new connection rather than try to reuse active connections. Default is not to.
    CurlFreshConnect Bool
  | -- | Do not reuse the connection of next transfer when done.
    CurlForbidReuse Bool
  | -- | Path to file used to seed (Open)SSL PRNG.
    CurlRandomFile FilePath
  | -- | Path to domain socket of EG Daemon.
    CurlEgdSocket FilePath
  | -- | max number of seconds to wait for the initial connection to happen.
    CurlConnectTimeout Long
  | -- | callback used to handle _incoming_ header data.
    CurlHeaderFunction WriteFunction
  | -- | Revert to a GET for the next request.
    CurlHttpGet Bool
  | -- | Perform Common name checking in peer certificate (1=> existence;2=> matches hostname.)
    CurlSSLVerifyHost Long
  | -- | Path to file where additional cookie information will be stored.
    CurlCookieJar FilePath
  | -- | Colon-separated string list of cipher preferences to use for upcoming connection (e.g., "3DES:+RSA")
    CurlSSLCipherList String
  | -- | What HTTP version to use, should you want to drop back for some reason.
    CurlHttpVersion HttpVersion
  | -- | Attempt the use of EPSV before PASV for passive FTP downloads.
    CurlFtpUseEPSV Bool
  | -- | The format of your certificates ("PEM", "DER")
    CurlSSLCertType String
  | -- | Filename of private key.
    CurlSSLKey FilePath
  | -- | Format of private key; use "ENG" to load from a crypto engine.
    CurlSSLKeyType String
  | -- | Name of crypto engine to use.
    CurlSSLEngine String
  | -- | Make crypto engine the default for crypto operations.
    CurlSSLEngineDefault
  | -- | Have library uses its MT-unfriendly DNS global cache.
    CurlDNSUseGlobalCache Bool
  | -- | Number of seconds to cache results of DNS lookups in memory.
    CurlDNSCacheTimeout Long
  | -- | FTP commands to issue after connection and transfer mode has been set.
    CurlPreQuote [String]
  | -- | callback to catch and report transfer operations.
    CurlDebugFunction DebugFunction
  | -- | state argument to pass to debug callback.
    CurlDebugData (Ptr ())
  | -- | Signal the start of a cookie session, ignoring previous session cookies.
    CurlCookieSession Bool
  | -- | Directory holding CA certificates; used when verifying peer certificate.
    CurlCAPath FilePath
  | -- | Turn (down, presumably) the buffers the received data is chunked up into (and reported to the WriteFunction.) A hint, library is free to ignore.
    CurlBufferSize Long
  | -- | Turn off use of signals internally.
    CurlNoSignal Bool
  | -- | Share handles are used for sharing data among concurrent Curl objects.
    CurlShare (Ptr ())
  | -- | What type of proxy to use.
    CurlProxyType Long
  | -- | What to report in the Accept-Encoding: header
    CurlEncoding String
  | -- | Data associated with a Curl handle.
    CurlPrivate (Ptr ())
  | -- | Alternatives to standard 200 OK response strings; whatever it takes, I suppose.
    CurlHttp200Aliases String
  | -- | Pass on user:pass when following redirects.
    CurlUnrestrictedAuth Bool
  | -- | For active FTP downloads, try using EPRT command over LPRT.
    CurlFtppUseEPRT Bool
  | -- | State your authentication preferences.
    CurlHttpAuth [HttpAuth]
  | -- | callback to handle setting up SSL connections; have the power to abort them.
    CurlSSLCtxFunction SSLCtxtFunction
  | -- | state argument to pass into the above callback.
    CurlSSLCtxData (Ptr ())
  | -- | Have remote directories be created if not already there
    CurlFtpCreateMissingDirs Bool
  | -- | What preferred authentication schemes to use wrt. proxy.
    CurlProxyAuth [HttpAuth]
  | -- | max number of seconds to wait for remote server to ACK commands.
    CurlFtpResponseTimeout Long
  | -- | Whether to resolve wrt IPv4 or IPv6.
    CurlIPResolve Long
  | -- | Limit the number of bytes you're willing to download.
    CurlMaxFileSize Long
  | -- | Wider alternative of option giving upper bound of uploaded content (-1 => unknown.)
    CurlInFileSizeLarge LLong
  | -- | Wider alternative for specifying initial transfer offset.
    CurlResumeFromLarge LLong
  | -- | Wider alternative for specifying max download size.
    CurlMaxFileSizeLarge LLong
  | -- | Path to user\'s .netrc
    CurlNetrcFile FilePath
  | -- | Try enabling the use of SSL for FTP control connections and\/or transfers.
    CurlFtpSSL Long
  | -- | Size of data to POST; if unspecified (or -1), curl uses strlen().
    CurlPostFieldSizeLarge LLong
  | -- | Turn on or off the TCP\/IP NODELAY option.
    CurlTCPNoDelay Bool
  | -- | Twiddle if TLS or SSL is used.
    CurlFtpSSLAuth Long
  | -- | somewhat obscure callback for handling read stream resets.
    CurlIOCTLFunction (Ptr ())
  | -- | state argument to the above.
    CurlIOCTLData (Ptr ())
  | -- | The string to use when server asks for account info.
    CurlFtpAccount String
  | -- | Cookie string to pass cookie engine; "ALL" scrubs all cookie info; "SESS" scrubs session ones.
    CurlCookieList String
  | -- | If Content-Length: values are troublesome (wrong, perhaps?), use this option to ignore using them as guidance.
    CurlIgnoreContentLength Bool
  | -- | Ignore IP address in 227 responses.
    CurlFtpSkipPASVIP Bool
  | -- | How to navigate to a file on the remote server (single, multiple CWDs).
    CurlFtpFileMethod Long
  | -- | What local port to use for established connection.
    CurlLocalPort Port
  | -- | Number of attempts at finding local ports (using LocalPort as initial base.)
    CurlLocalPortRange Port
  | -- | If enabled, perform all steps up until actual transfer.
    CurlConnectOnly Bool
  | -- | callback for doing character translations from network format.
    CurlConvFromNetworkFunction (Ptr ())
  | -- | callback for doing character translations to network format.
    CurlConvToNetworkFunction (Ptr ())
  | -- | callback for translating UTF8 into host encoding.
    CurlConvFromUtf8Function (Ptr ())
  | -- | Specifies throttle value for outgoing data.
    CurlMaxSendSpeedLarge LLong
  | -- | Specifies throttle for incoming data.
    CurlMaxRecvSpeedLarge LLong
  | -- | Alternative (to user:pass) for FTP authentication; weird.
    CurlFtpAlternativeToUser String
  | -- | callback that's injected between socket creation and connection.
    CurlSockOptFunction (Ptr ())
  | -- | state argument to the above.
    CurlSockOptData (Ptr ())
  | -- | Enable the SSL session id cache; default is on, so use this to disable.
    CurlSSLSessionIdCache Bool
  | -- | SSH authentication methods to use.
    CurlSSHAuthTypes [SSHAuthType]
  | -- | Path to file holding user's SSH public key.
    CurlSSHPublicKeyFile FilePath
  | -- | Path to file holding user's SSH private key.
    CurlSSHPrivateKeyFile FilePath
  | -- | Send CCC command after FTP connection has been authenticated.
    CurlFtpSSLCCC Bool
  | -- | Max number of milliseconds that a transfer may take.
    CurlTimeoutMS Long
  | -- | Max number of milliseconds that a connection attempt may take to complete.
    CurlConnectTimeoutMS Long
  | -- | Disable transfer decoding; if disabled, curl will turn off chunking.
    CurlHttpTransferDecoding Bool
  | -- | Disable content decoding, getting the raw bits.
    CurlHttpContentDecoding Bool
  | CurlNewFilePerms Long
  | CurlNewDirectoryPerms Long
  | CurlPostRedirect Bool
  | CurlSSHHostPublicKeyMD5 String
  | CurlCopyPostFields Bool
  | CurlProxyTransferMode Long
  | CurlCRLFile FilePath
  | CurlIssuerCert FilePath
  | CurlAddressScope Long
  | CurlCertInfo Long
  | CurlUserName String
  | CurlUserPassword String
  | CurlProxyUser String
  | CurlProxyPassword String

instance Show CurlOption where
  show x = showCurlOption x

data HttpVersion
  = HttpVersionNone
  | HttpVersion10
  | HttpVersion11
  deriving (Enum, Show)

data TimeCond
  = TimeCondNone
  | TimeCondIfModSince
  | TimeCondIfUnmodSince
  | TimeCondLastMode
  deriving (Enum, Show)

data NetRcOption
  = NetRcIgnored
  | NetRcOptional
  | NetRcRequired
  deriving (Enum, Show)

data HttpAuth
  = HttpAuthNone
  | HttpAuthBasic
  | HttpAuthDigest
  | HttpAuthGSSNegotiate
  | HttpAuthNTLM
  | HttpAuthAny
  | HttpAuthAnySafe
  deriving (Enum, Show)

toHttpAuthMask :: [HttpAuth] -> Long
toHttpAuthMask [] = 0
toHttpAuthMask (x : xs) =
  let vs = toHttpAuthMask xs
   in case x of
        HttpAuthNone -> vs
        HttpAuthBasic -> 0x1 .|. vs
        HttpAuthDigest -> 0x2 .|. vs
        HttpAuthGSSNegotiate -> 0x4 .|. vs
        HttpAuthNTLM -> 0x8 .|. vs
        HttpAuthAny -> (complement 0) .|. vs
        HttpAuthAnySafe -> (complement 1) .|. vs

data SSHAuthType
  = SSHAuthAny
  | SSHAuthNone
  | SSHAuthPublickey
  | SSHAuthPassword
  | SSHAuthHost
  | SSHAuthKeyboard
  deriving (Show)

toSSHAuthMask :: [SSHAuthType] -> Long
toSSHAuthMask [] = 0
toSSHAuthMask (x : xs) =
  let vs = toSSHAuthMask xs
   in case x of
        SSHAuthAny -> (complement 0) .|. vs
        SSHAuthNone -> vs
        SSHAuthPublickey -> 1 .|. vs
        SSHAuthPassword -> 2 .|. vs
        SSHAuthHost -> 4 .|. vs
        SSHAuthKeyboard -> 8 .|. vs

type WriteFunction =
  Ptr CChar -> --  pointer to external buffer holding data
  CInt -> --  width (in bytes) of each item
  CInt -> --  number of items
  Ptr () -> --  state argument (file pointer etc.)
  IO CInt --  number of bytes written.

type ReadFunction =
  Ptr CChar -> --  pointer to external buffer to fill in.
  CInt -> --  width (in bytes) of each item
  CInt -> --  number of items
  Ptr () -> --  state argument (file pointer etc.)
  IO (Maybe CInt) --  how many bytes was copied into buffer; Nothing => abort.

type ReadFunctionPrim =
  Ptr CChar ->
  CInt ->
  CInt ->
  Ptr () ->
  IO CInt

type ProgressFunction =
  Ptr () -> --  state argument
  Double -> --  expected download totals
  Double -> --  download totals so far
  Double -> --  expected upload totals
  Double -> --  upload totals so far
  IO CInt --  not sure; 0 is a good one.

type DebugFunction =
  Curl -> --  connection handle
  DebugInfo -> --  type of call
  Ptr CChar -> --  data buffer
  CInt -> --  length of buffer
  Ptr () -> --  state argument
  IO () --  always 0

data DebugInfo
  = InfoText
  | InfoHeaderIn
  | InfoHeaderOut
  | InfoDataIn
  | InfoDataOut
  | InfoSslDataIn
  | InfoSslDataOut
  deriving (Eq, Enum)

type DebugFunctionPrim =
  CurlH -> --  connection handle
  CInt -> --  type of call
  Ptr CChar -> --  data buffer
  CInt -> --  length of buffer
  Ptr () -> --  state argument
  IO CInt --  always 0

type SSLCtxtFunction =
  CurlH -> --  connection handle
  Ptr () -> --  the SSL_CTX handle
  Ptr () -> --  state argument
  IO CInt

curl_readfunc_abort :: CInt
curl_readfunc_abort = 0x10000000

baseLong :: Int
baseLong = 0

baseObject :: Int
baseObject = 10000

baseFunction :: Int
baseFunction = 20000

baseOffT :: Int
baseOffT = 30000

unmarshallOption :: Unmarshaller a -> CurlOption -> IO a
unmarshallOption um c =
  let l = (baseLong +)
      o = (baseObject +)
      f = (baseFunction +)
      off = (baseOffT +)
   in case c of
        CurlFileObj x -> u_ptr um (o 1) x
        CurlURL x -> u_string um (o 2) x
        CurlPort x -> u_long um (l 3) x
        CurlProxy x -> u_string um (o 4) x
        CurlUserPwd x -> u_string um (o 5) x
        CurlProxyUserPwd x -> u_string um (o 6) x
        CurlRange x -> u_string um (o 7) x
        CurlInFile x -> u_string um (o 9) x
        CurlErrorBuffer x -> u_cptr um (o 10) x
        CurlWriteFunction x -> u_writeFun um (f 11) x
        CurlReadFunction x -> u_readFun um (f 12) x
        CurlTimeout x -> u_long um (l 13) x
        CurlInFileSize x -> u_long um (l 14) x
        CurlPostFields x -> u_string um (o 15) (concat $ intersperse "&" x)
        CurlReferer x -> u_string um (o 16) x
        CurlFtpPort x -> u_string um (o 17) x
        CurlUserAgent x -> u_string um (o 18) x
        CurlLowSpeed x -> u_long um (l 19) x
        CurlLowSpeedTime x -> u_long um (l 20) x
        CurlResumeFrom x -> u_long um (l 21) x
        CurlCookie x -> u_string um (o 22) x
        CurlHttpHeaders x -> u_strings um (o 23) x
        CurlHttpPost x -> u_posts um (o 24) x
        CurlSSLCert x -> u_string um (o 25) x
        CurlSSLPassword x -> u_string um (o 26) x
        CurlSSLKeyPassword x -> u_string um (o 26) x -- yes, duplicate.
        CurlCRLF x -> u_bool um (l 27) x
        CurlQuote x -> u_strings um (o 28) x
        CurlWriteHeader x -> u_ptr um (o 29) x
        CurlCookieFile x -> u_string um (o 31) x
        CurlSSLVersion x -> u_long um (l 32) x
        CurlTimeCondition x -> u_enum um (l 33) x
        CurlTimeValue x -> u_long um (l 34) x
        CurlCustomRequest x -> u_string um (o 36) x
        -- CurlStderr x -> u_string um (o 37) x
        CurlPostQuote x -> u_strings um (o 39) x
        CurlWriteInfo x -> u_string um (o 40) x
        CurlVerbose x -> u_bool um (l 41) x
        CurlHeader x -> u_bool um (l 42) x
        CurlNoProgress x -> u_bool um (l 43) x
        CurlNoBody x -> u_bool um (l 44) x
        CurlFailOnError x -> u_bool um (l 45) x
        CurlUpload x -> u_bool um (l 46) x
        CurlPost x -> u_bool um (l 47) x
        CurlFtpListOnly x -> u_bool um (l 48) x
        CurlFtpAppend x -> u_bool um (l 50) x
        CurlUseNetRc x -> u_enum um (l 51) x
        CurlFollowLocation x -> u_bool um (l 52) x
        CurlTransferTextASCII x -> u_bool um (l 53) x
        CurlPut x -> u_bool um (l 54) x
        CurlProgressFunction x -> u_progressFun um (f 56) x
        CurlProgressData x -> u_ptr um (o 57) x
        CurlAutoReferer x -> u_bool um (l 58) x
        CurlProxyPort x -> u_long um (l 59) x
        CurlPostFieldSize x -> u_long um (l 60) x
        CurlHttpProxyTunnel x -> u_bool um (l 61) x
        CurlInterface x -> u_string um (o 62) x
        CurlKrb4Level x -> u_string um (o 63) x
        CurlSSLVerifyPeer x -> u_bool um (l 64) x
        CurlCAInfo x -> u_string um (o 65) x
        CurlMaxRedirs x -> u_long um (l 68) x
        CurlFiletime x -> u_bool um (l 69) x
        CurlTelnetOptions x -> u_strings um (o 70) x
        CurlMaxConnects x -> u_long um (l 71) x
        CurlClosePolicy x -> u_long um (l 72) x
        CurlFreshConnect x -> u_bool um (l 74) x
        CurlForbidReuse x -> u_bool um (l 75) x
        CurlRandomFile x -> u_string um (o 76) x
        CurlEgdSocket x -> u_string um (o 77) x
        CurlConnectTimeout x -> u_long um (l 78) x
        CurlHeaderFunction x -> u_writeFun um (f 79) x
        CurlHttpGet x -> u_bool um (l 80) x
        CurlSSLVerifyHost x -> u_long um (l 81) x
        CurlCookieJar x -> u_string um (o 82) x
        CurlSSLCipherList x -> u_string um (o 83) x -- a string (or a l-list of them)?
        CurlHttpVersion x -> u_enum um (l 84) x
        CurlFtpUseEPSV x -> u_bool um (l 85) x
        CurlSSLCertType x -> u_string um (o 86) x
        CurlSSLKey x -> u_string um (o 87) x
        CurlSSLKeyType x -> u_string um (o 88) x
        CurlSSLEngine x -> u_string um (o 89) x
        CurlSSLEngineDefault -> u_bool um (l 90) True
        CurlDNSUseGlobalCache x -> u_bool um (l 91) x
        CurlDNSCacheTimeout x -> u_long um (l 92) x
        CurlPreQuote x -> u_strings um (o 93) x
        CurlDebugFunction x -> u_debugFun um (f 94) x
        CurlDebugData x -> u_ptr um (o 95) x
        CurlCookieSession x -> u_bool um (l 96) x
        CurlCAPath x -> u_string um (o 97) x
        CurlBufferSize x -> u_long um (l 98) x
        CurlNoSignal x -> u_bool um (l 99) x
        CurlShare x -> u_ptr um (o 100) x
        CurlProxyType x -> u_enum um (l 101) x
        CurlEncoding x -> u_string um (o 102) x
        CurlPrivate x -> u_ptr um (o 103) x
        CurlHttp200Aliases x -> u_string um (o 104) x -- correct?
        CurlUnrestrictedAuth x -> u_bool um (l 105) x
        CurlFtppUseEPRT x -> u_bool um (l 106) x
        CurlHttpAuth xs -> u_long um (l 107) (toHttpAuthMask xs)
        CurlSSLCtxFunction x -> u_sslctxt um (f 108) x
        CurlSSLCtxData x -> u_ptr um (o 109) x
        CurlFtpCreateMissingDirs x -> u_bool um (l 110) x
        CurlProxyAuth x -> u_long um (l 111) (toHttpAuthMask x)
        CurlFtpResponseTimeout x -> u_long um (l 112) x
        CurlIPResolve x -> u_long um (l 113) x
        CurlMaxFileSize x -> u_long um (l 114) x
        CurlInFileSizeLarge x -> u_llong um (off 115) x
        CurlResumeFromLarge x -> u_llong um (off 116) x
        CurlMaxFileSizeLarge x -> u_llong um (off 117) x
        CurlNetrcFile x -> u_string um (o 118) x
        CurlFtpSSL x -> u_enum um (l 119) x
        CurlPostFieldSizeLarge x -> u_llong um (off 120) x
        CurlTCPNoDelay x -> u_bool um (l 121) x
        CurlFtpSSLAuth x -> u_enum um (l 129) x
        CurlIOCTLFunction x -> u_ioctl_fun um (f 130) x
        CurlIOCTLData x -> u_ptr um (o 131) x
        CurlFtpAccount x -> u_string um (o 134) x
        CurlCookieList x -> u_string um (o 135) x
        CurlIgnoreContentLength x -> u_bool um (l 136) x
        CurlFtpSkipPASVIP x -> u_bool um (l 137) x
        CurlFtpFileMethod x -> u_enum um (l 138) x
        CurlLocalPort x -> u_long um (l 139) x
        CurlLocalPortRange x -> u_long um (l 140) x
        CurlConnectOnly x -> u_bool um (l 141) x
        CurlConvFromNetworkFunction x -> u_convFromNetwork um (f 142) x
        CurlConvToNetworkFunction x -> u_convToNetwork um (f 143) x
        CurlConvFromUtf8Function x -> u_convFromUtf8 um (f 144) x
        CurlMaxSendSpeedLarge x -> u_llong um (off 145) x
        CurlMaxRecvSpeedLarge x -> u_llong um (off 146) x
        CurlFtpAlternativeToUser x -> u_string um (o 147) x
        CurlSockOptFunction x -> u_sockoptFun um (f 148) x
        CurlSockOptData x -> u_ptr um (o 149) x
        CurlSSLSessionIdCache x -> u_bool um (l 150) x
        CurlSSHAuthTypes xs -> u_long um (l 151) (toSSHAuthMask xs)
        CurlSSHPublicKeyFile x -> u_string um (o 152) x
        CurlSSHPrivateKeyFile x -> u_string um (o 153) x
        CurlFtpSSLCCC x -> u_bool um (l 154) x
        CurlTimeoutMS x -> u_long um (l 155) x
        CurlConnectTimeoutMS x -> u_long um (l 156) x
        CurlHttpTransferDecoding x -> u_bool um (l 157) x
        CurlHttpContentDecoding x -> u_bool um (l 158) x
        CurlNewFilePerms x -> u_long um (l 159) x
        CurlNewDirectoryPerms x -> u_long um (l 160) x
        CurlPostRedirect x -> u_bool um (l 161) x
        CurlSSHHostPublicKeyMD5 x -> u_string um (l 162) x
        CurlCopyPostFields x -> u_bool um (l 165) x
        CurlProxyTransferMode x -> u_long um (l 166) x
        CurlCRLFile x -> u_string um (l 169) x
        CurlIssuerCert x -> u_string um (l 170) x
        CurlAddressScope x -> u_long um (l 171) x
        CurlCertInfo x -> u_long um (l 172) x
        CurlUserName x -> u_string um (l 173) x
        CurlUserPassword x -> u_string um (l 174) x
        CurlProxyUser x -> u_string um (l 175) x
        CurlProxyPassword x -> u_string um (l 176) x

data Unmarshaller a = Unmarshaller
  { u_long :: Int -> Long -> IO a,
    u_llong :: Int -> LLong -> IO a,
    u_string :: Int -> String -> IO a,
    u_strings :: Int -> [String] -> IO a,
    u_ptr :: Int -> Ptr () -> IO a,
    u_writeFun :: Int -> WriteFunction -> IO a,
    u_readFun :: Int -> ReadFunction -> IO a,
    u_progressFun :: Int -> ProgressFunction -> IO a,
    u_debugFun :: Int -> DebugFunction -> IO a,
    u_posts :: Int -> [HttpPost] -> IO a,
    u_sslctxt :: Int -> SSLCtxtFunction -> IO a,
    u_ioctl_fun :: Int -> Ptr () -> IO a,
    u_convFromNetwork :: Int -> Ptr () -> IO a,
    u_convToNetwork :: Int -> Ptr () -> IO a,
    u_convFromUtf8 :: Int -> Ptr () -> IO a,
    u_sockoptFun :: Int -> Ptr () -> IO a
  }

verboseUnmarshaller :: Unmarshaller a -> Unmarshaller a
verboseUnmarshaller u =
  let two m f x y = putStrLn m >> f u x y
      twoS m f x y = putStrLn (m ++ ": " ++ show (x, y)) >> f u x y
   in u
        { u_long = twoS "u_long" u_long,
          u_llong = twoS "u_llong" u_llong,
          u_string = twoS "u_string" u_string,
          u_strings = twoS "u_strings" u_strings,
          u_ptr = twoS "u_ptr" u_ptr,
          u_writeFun = two "u_writeFun" u_writeFun,
          u_readFun = two "u_readFun" u_readFun,
          u_progressFun = two "u_progressFun" u_progressFun,
          u_debugFun = two "u_debugFun" u_debugFun,
          u_posts = two "u_posts" u_posts,
          u_sslctxt = two "u_sslctxt" u_sslctxt,
          u_ioctl_fun = two "u_ioctl_fun" u_ioctl_fun,
          u_convFromNetwork = twoS "u_convFromNetwork" u_convFromNetwork,
          u_convToNetwork = twoS "u_convToNetwork" u_convToNetwork,
          u_convFromUtf8 = twoS "u_convFromUtf8" u_convFromUtf8,
          u_sockoptFun = twoS "u_sockoptFun" u_sockoptFun
        }

u_bool :: Unmarshaller a -> Int -> Bool -> IO a
u_bool um x b = u_long um x (if b then 1 else 0)

u_enum :: Enum b => Unmarshaller a -> Int -> b -> IO a
u_enum um x b = u_long um x (fromIntegral $ fromEnum b)

u_cptr :: Unmarshaller a -> Int -> Ptr CChar -> IO a
u_cptr um x p = u_ptr um x (castPtr p)

showCurlOption :: CurlOption -> String
showCurlOption o =
  case o of
    CurlFileObj p -> "CurlFileObj " ++ show p
    CurlURL u -> "CurlURL " ++ show u
    CurlPort p -> "CurlPort " ++ show p
    CurlProxy s -> "CurlProxy " ++ show s
    CurlUserPwd p -> "CurlUserPwd " ++ show p
    CurlProxyUserPwd p -> "CurlProxyUserPwd " ++ show p
    CurlRange p -> "CurlRange " ++ show p
    CurlInFile p -> "CurlInFile " ++ show p
    CurlErrorBuffer p -> "CurlErrorBuffer " ++ show p
    CurlWriteFunction {} -> "CurlWriteFunction <fun>"
    CurlReadFunction {} -> "CurlReadFunction <fun>"
    CurlTimeout l -> "CurlTimeout " ++ show l
    CurlInFileSize l -> "CurlInFileSize " ++ show l
    CurlPostFields p -> "CurlPostFields " ++ show p
    CurlReferer p -> "CurlReferer " ++ show p
    CurlFtpPort p -> "CurlFtpPort " ++ show p
    CurlUserAgent p -> "CurlUserAgent " ++ show p
    CurlLowSpeed p -> "CurlLowSpeed " ++ show p
    CurlLowSpeedTime p -> "CurlLowSpeedTime " ++ show p
    CurlResumeFrom p -> "CurlResumeFrom " ++ show p
    CurlCookie p -> "CurlCookie " ++ show p
    CurlHttpHeaders p -> "CurlHttpHeaders " ++ show p
    CurlHttpPost p -> "CurlHttpPost " ++ show p
    CurlSSLCert p -> "CurlSSLCert " ++ show p
    CurlSSLPassword p -> "CurlSSLPassword " ++ show p
    CurlSSLKeyPassword p -> "CurlSSLKeyPassword " ++ show p
    CurlCRLF p -> "CurlCRLF " ++ show p
    CurlQuote p -> "CurlQuote " ++ show p
    CurlWriteHeader p -> "CurlWriteHeader " ++ show p
    CurlCookieFile p -> "CurlCookieFile " ++ show p
    CurlSSLVersion p -> "CurlSSLVersion " ++ show p
    CurlTimeCondition p -> "CurlTimeCondition " ++ show p
    CurlTimeValue p -> "CurlTimeValue " ++ show p
    CurlCustomRequest p -> "CurlCustomRequest " ++ show p
    CurlPostQuote p -> "CurlPostQuote " ++ show p
    CurlWriteInfo p -> "CurlWriteInfo " ++ show p
    CurlVerbose p -> "CurlVerbose " ++ show p
    CurlHeader p -> "CurlHeader " ++ show p
    CurlNoProgress p -> "CurlNoProgress " ++ show p
    CurlNoBody p -> "CurlNoBody " ++ show p
    CurlFailOnError p -> "CurlFailOnError " ++ show p
    CurlUpload p -> "CurlUpload " ++ show p
    CurlPost p -> "CurlPost " ++ show p
    CurlFtpListOnly p -> "CurlFtpListOnly " ++ show p
    CurlFtpAppend p -> "CurlFtpAppend " ++ show p
    CurlUseNetRc p -> "CurlUseNetRc " ++ show p
    CurlFollowLocation p -> "CurlFollowLocation " ++ show p
    CurlTransferTextASCII p -> "CurlTransferTextASCII " ++ show p
    CurlPut p -> "CurlPut " ++ show p
    CurlProgressFunction {} -> "CurlProgressFunction <fun>"
    CurlProgressData p -> "CurlProgressData " ++ show p
    CurlAutoReferer p -> "CurlAutoReferer " ++ show p
    CurlProxyPort p -> "CurlProxyPort " ++ show p
    CurlPostFieldSize p -> "CurlPostFieldSize " ++ show p
    CurlHttpProxyTunnel p -> "CurlHttpProxyTunnel " ++ show p
    CurlInterface p -> "CurlInterface " ++ show p
    CurlKrb4Level p -> "CurlKrb4Level " ++ show p
    CurlSSLVerifyPeer p -> "CurlSSLVerifyPeer " ++ show p
    CurlCAInfo p -> "CurlCAInfo " ++ show p
    CurlMaxRedirs p -> "CurlMaxRedirs " ++ show p
    CurlFiletime p -> "CurlFiletime " ++ show p
    CurlTelnetOptions p -> "CurlTelnetOptions " ++ show p
    CurlMaxConnects p -> "CurlMaxConnects " ++ show p
    CurlClosePolicy p -> "CurlClosePolicy " ++ show p
    CurlFreshConnect p -> "CurlFreshConnect " ++ show p
    CurlForbidReuse p -> "CurlForbidReuse " ++ show p
    CurlRandomFile p -> "CurlRandomFile " ++ show p
    CurlEgdSocket p -> "CurlEgdSocket " ++ show p
    CurlConnectTimeout p -> "CurlConnectTimeout " ++ show p
    CurlHeaderFunction {} -> "CurlHeaderFunction <fun>"
    CurlHttpGet p -> "CurlHttpGet " ++ show p
    CurlSSLVerifyHost p -> "CurlSSLVerifyHost " ++ show p
    CurlCookieJar p -> "CurlCookieJar " ++ show p
    CurlSSLCipherList p -> "CurlSSLCipherList " ++ show p
    CurlHttpVersion p -> "CurlHttpVersion " ++ show p
    CurlFtpUseEPSV p -> "CurlFtpUseEPSV " ++ show p
    CurlSSLCertType p -> "CurlSSLCertType " ++ show p
    CurlSSLKey p -> "CurlSSLKey " ++ show p
    CurlSSLKeyType p -> "CurlSSLKeyType " ++ show p
    CurlSSLEngine p -> "CurlSSLEngine " ++ show p
    CurlSSLEngineDefault -> "CurlSSLEngineDefault"
    CurlDNSUseGlobalCache p -> "CurlDNSUseGlobalCache " ++ show p
    CurlDNSCacheTimeout p -> "CurlDNSCacheTimeout " ++ show p
    CurlPreQuote p -> "CurlPreQuote " ++ show p
    CurlDebugFunction {} -> "CurlDebugFunction <fun>"
    CurlDebugData p -> "CurlDebugData " ++ show p
    CurlCookieSession p -> "CurlCookieSession " ++ show p
    CurlCAPath p -> "CurlCAPath " ++ show p
    CurlBufferSize p -> "CurlBufferSize " ++ show p
    CurlNoSignal p -> "CurlNoSignal " ++ show p
    CurlShare p -> "CurlShare " ++ show p
    CurlProxyType p -> "CurlProxyType " ++ show p
    CurlEncoding p -> "CurlEncoding " ++ show p
    CurlPrivate p -> "CurlPrivate " ++ show p
    CurlHttp200Aliases p -> "CurlHttp200Aliases " ++ show p
    CurlUnrestrictedAuth p -> "CurlUnrestrictedAuth " ++ show p
    CurlFtppUseEPRT p -> "CurlFtppUseEPRT " ++ show p
    CurlHttpAuth p -> "CurlHttpAuth " ++ show p
    CurlSSLCtxFunction {} -> "CurlSSLCtxFunction <fun>"
    CurlSSLCtxData p -> "CurlSSLCtxData " ++ show p
    CurlFtpCreateMissingDirs p -> "CurlFtpCreateMissingDirs " ++ show p
    CurlProxyAuth p -> "CurlProxyAuth " ++ show p
    CurlFtpResponseTimeout p -> "CurlFtpResponseTimeout " ++ show p
    CurlIPResolve p -> "CurlIPResolve " ++ show p
    CurlMaxFileSize p -> "CurlMaxFileSize " ++ show p
    CurlInFileSizeLarge p -> "CurlInFileSizeLarge " ++ show p
    CurlResumeFromLarge p -> "CurlResumeFromLarge " ++ show p
    CurlMaxFileSizeLarge p -> "CurlMaxFileSizeLarge " ++ show p
    CurlNetrcFile p -> "CurlNetrcFile " ++ show p
    CurlFtpSSL p -> "CurlFtpSSL " ++ show p
    CurlPostFieldSizeLarge p -> "CurlPostFieldSizeLarge " ++ show p
    CurlTCPNoDelay p -> "CurlTCPNoDelay " ++ show p
    CurlFtpSSLAuth p -> "CurlFtpSSLAuth " ++ show p
    CurlIOCTLFunction p -> "CurlIOCTLFunction " ++ show p
    CurlIOCTLData p -> "CurlIOCTLData " ++ show p
    CurlFtpAccount p -> "CurlFtpAccount " ++ show p
    CurlCookieList p -> "CurlCookieList " ++ show p
    CurlIgnoreContentLength p -> "CurlIgnoreContentLength " ++ show p
    CurlFtpSkipPASVIP p -> "CurlFtpSkipPASVIP " ++ show p
    CurlFtpFileMethod p -> "CurlFtpFileMethod " ++ show p
    CurlLocalPort p -> "CurlLocalPort " ++ show p
    CurlLocalPortRange p -> "CurlLocalPortRange " ++ show p
    CurlConnectOnly p -> "CurlConnectOnly " ++ show p
    CurlConvFromNetworkFunction p -> "CurlConvFromNetworkFunction " ++ show p
    CurlConvToNetworkFunction p -> "CurlConvToNetworkFunction " ++ show p
    CurlConvFromUtf8Function p -> "CurlConvFromUtf8Function " ++ show p
    CurlMaxSendSpeedLarge p -> "CurlMaxSendSpeedLarge " ++ show p
    CurlMaxRecvSpeedLarge p -> "CurlMaxRecvSpeedLarge " ++ show p
    CurlFtpAlternativeToUser p -> "CurlFtpAlternativeToUser " ++ show p
    CurlSockOptFunction p -> "CurlSockOptFunction " ++ show p
    CurlSockOptData p -> "CurlSockOptData " ++ show p
    CurlSSLSessionIdCache p -> "CurlSSLSessionIdCache " ++ show p
    CurlSSHAuthTypes p -> "CurlSSHAuthTypes " ++ show p
    CurlSSHPublicKeyFile p -> "CurlSSHPublicKeyFile " ++ show p
    CurlSSHPrivateKeyFile p -> "CurlSSHPrivateKeyFile " ++ show p
    CurlFtpSSLCCC p -> "CurlFtpSSLCCC " ++ show p
    CurlTimeoutMS p -> "CurlTimeoutMS " ++ show p
    CurlConnectTimeoutMS p -> "CurlConnectTimeoutMS " ++ show p
    CurlHttpTransferDecoding p -> "CurlHttpTransferDecoding " ++ show p
    CurlHttpContentDecoding p -> "CurlHttpContentDecoding " ++ show p
    CurlNewFilePerms l -> "CurlNewFilePerms " ++ show l
    CurlNewDirectoryPerms p -> "CurlNewDirectoryPerms " ++ show p
    CurlPostRedirect p -> "CurlPostRedirect " ++ show p
    CurlSSHHostPublicKeyMD5 p -> "CurlSSHHostPublicKeyMD5 " ++ show p
    CurlCopyPostFields p -> "CurlCopyPostFields " ++ show p
    CurlProxyTransferMode p -> "CurlProxyTransferMode " ++ show p
    CurlCRLFile p -> "CurlCRLFile " ++ show p
    CurlIssuerCert p -> "CurlIssuerCert " ++ show p
    CurlAddressScope p -> "CurlAddressScope " ++ show p
    CurlCertInfo p -> "CurlCertInfo " ++ show p
    CurlUserName p -> "CurlUserName " ++ show p
    CurlUserPassword p -> "CurlUserPassword " ++ show p
    CurlProxyUser p -> "CurlProxyUser " ++ show p
    CurlProxyPassword p -> "CurlProxyPassword " ++ show p
