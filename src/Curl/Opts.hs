module Curl.Opts (module O) where

import Curl.Internal.Opts as O
  ( CurlOption (..),
    DebugFunction (..),
    DebugInfo (..),
    HttpAuth (..),
    HttpVersion (..),
    NetRcOption (..),
    ProgressFunction (..),
    ReadFunction (..),
    SshAuthType (..),
    SslCtxtFunction (..),
    TimeCond (..),
    WriteFunction (..),
    pattern GET,
    pattern HEAD,
    pattern POST,
  )
import Curl.Internal.Post as O
  ( Content (..),
    Header,
    HttpPost (..),
    multiformString,
  )
