module Curl.Opts (module O) where

import Curl.Internal.Opts as O
  ( CurlOption (..),
    DebugFunction (..),
    DebugInfo (..),
    File,
    HttpAuth (..),
    HttpVersion (..),
    NetRcOption (..),
    ProgressFunction (..),
    ReadFunction (..),
    SshAuthType (..),
    SslCtxtFunction (..),
    TimeCond (..),
    WriteFunction (..),
    fclose,
    fflush,
    fopen,
    pattern GET,
    pattern HEAD,
    pattern POST,
  )
import Curl.Internal.Post as O
  ( Content (..),
    HttpPost (..),
    multiformString,
  )
import Network.HTTP.Types as O (ByteRange (..))
import URI.ByteString as O (UserInfo (..))
