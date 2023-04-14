-- |
-- Module    : Curl
-- Copyright : (c) 2007-2009, Galois Inc
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- A Haskell binding the libcurl library <http://curl.haxx.se/>, a
-- proven and feature-rich library for interacting with HTTP(S)\/FTP
-- servers.
--
-- The binding was initially made against version 7.16.2; libcurl does
-- appear to be considerate in not introducing breaking changes wrt
-- older versions. So, unless you're after the latest features (i.e.,
-- constructors towards the end the Option type), there's a very good
-- chance your code will work against older installations of libcurl.
module Curl where

import Control.Monad (void)
import Curl.Easy
import qualified Curl.Internal as Internal
import Curl.Opts
import Curl.Post (HttpPost)
import Curl.Types
import Data.ByteString.Lazy (ByteString)

-- | Run a @GET@ request and collect the body as a lazy 'ByteString'
get :: UrlString -> [CurlOption] -> IO (CurlCode, ByteString)
get url = runCurl . Internal.curlGetString url

-- | Run a @GET@ request, dumping the output on stdout
get_ :: UrlString -> [CurlOption] -> IO CurlCode
get_ url = runCurl . Internal.curlGet url

-- | Run a @HEAD@ request, gathering response info into a 'CurlResponse'
head :: UrlString -> [CurlOption] -> IO CurlResponse
head url = runCurl . Internal.curlHead url

-- | Run a @POST@ request, returning the 'CurlCode'
post :: UrlString -> [String] -> IO CurlCode
post url = runCurl . Internal.curlPost url

-- | Run a @POST@ request, discarding the final 'CurlCode'
post_ :: UrlString -> [String] -> IO ()
post_ url = void . runCurl . Internal.curlPost url

-- | Run a multipart @POST@ request, returning the 'CurlCode'
mulitpart :: UrlString -> [CurlOption] -> [HttpPost] -> IO CurlCode
mulitpart url opts = runCurl . Internal.curlMultipart url opts

-- | Run a multipart @POST@ request, discarding the final 'CurlCode'
mulitpart_ :: UrlString -> [CurlOption] -> [HttpPost] -> IO ()
mulitpart_ url opts = void . runCurl . Internal.curlMultipart url opts

-- | Run a custom curl request (as specified by the 'CurlOption's), returning
-- the 'CurlReponse'
runWithResponse :: UrlString -> [CurlOption] -> IO CurlResponse
runWithResponse url = runCurl . Internal.runWithResponse url

-- | Run a custom curl request (as specified by the 'CurlOption's), returning
-- the 'CurlReponse'. Also includes the 'InfoValue's taken from the provided
-- 'Info's
runWithResponseInfo :: UrlString -> [CurlOption] -> [Info] -> IO CurlResponse
runWithResponseInfo url opts = runCurl . Internal.runWithResponseInfo url opts
