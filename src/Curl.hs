module Curl
  ( get,
    get_,
    head,
    post,
    post_,
    multipart,
    multipart_,
    runWithResponse,
    runWithResponseInfo,
    module M,
  )
where

import Control.Monad (void)
import qualified Curl.Internal as Internal
import Curl.Internal.Easy
import Curl.Internal.Opts
import Curl.Internal.Types
import Curl.Opts as M
import Curl.Types as M hiding (CookieList, Filetime, Private) -- conflicts
import Data.ByteString.Lazy (ByteString)
import Prelude hiding (head)

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
multipart :: UrlString -> [CurlOption] -> [HttpPost] -> IO CurlCode
multipart url opts = runCurl . Internal.curlMultipart url opts

-- | Run a multipart @POST@ request, discarding the final 'CurlCode'
multipart_ :: UrlString -> [CurlOption] -> [HttpPost] -> IO ()
multipart_ url opts = void . runCurl . Internal.curlMultipart url opts

-- | Run a custom curl request (as specified by the 'CurlOption's), returning
-- the 'CurlReponse'
runWithResponse :: UrlString -> [CurlOption] -> IO CurlResponse
runWithResponse url = runCurl . Internal.runWithResponse url

-- | Run a custom curl request (as specified by the 'CurlOption's), returning
-- the 'CurlReponse'. Also includes the 'InfoValue's taken from the provided
-- 'Info's
runWithResponseInfo :: UrlString -> [CurlOption] -> [Info] -> IO CurlResponse
runWithResponseInfo url opts = runCurl . Internal.runWithResponseInfo url opts
