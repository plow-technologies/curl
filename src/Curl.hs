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
    withFilePtr,
    module M,
  )
where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Curl.Internal as M (callbackWriter, ignoreOutput)
import qualified Curl.Internal as Internal
import Curl.Internal.Easy
import Curl.Internal.Opts
import Curl.Internal.Types
import Curl.Opts as M
import Curl.Types as M hiding (CookieList, Filetime, Private)
import Data.ByteString.Lazy (ByteString)
import Foreign.Ptr (Ptr)
import Prelude hiding (head)

-- | Run a @GET@ request and collect the body as a lazy 'ByteString'
-- NOTE: This sets the 'WriteFun' option!
get :: MonadIO m => UrlString -> [CurlOption] -> m (CurlCode, ByteString)
get url = liftIO . runCurl . Internal.curlGetString url

-- | Run a @GET@ request, dumping the output on stdout
get_ :: MonadIO m => UrlString -> [CurlOption] -> m CurlCode
get_ url = liftIO . runCurl . Internal.curlGet url

-- | Run a @HEAD@ request, gathering response info into a 'CurlResponse'
head :: MonadIO m => UrlString -> [CurlOption] -> m CurlResponse
head url = liftIO . runCurl . Internal.curlHead url

-- | Run a @POST@ request, returning the 'CurlCode'
post :: MonadIO m => UrlString -> [String] -> m CurlCode
post url = liftIO . runCurl . Internal.curlPost url

-- | Run a @POST@ request, discarding the final 'CurlCode'
post_ :: MonadIO m => UrlString -> [String] -> m ()
post_ url = liftIO . void . runCurl . Internal.curlPost url

-- | Run a multipart @POST@ request, returning the 'CurlCode'
multipart :: MonadIO m => UrlString -> [CurlOption] -> [HttpPost] -> m CurlCode
multipart url opts = liftIO . runCurl . Internal.curlMultipart url opts

-- | Run a multipart @POST@ request, discarding the final 'CurlCode'
multipart_ :: MonadIO m => UrlString -> [CurlOption] -> [HttpPost] -> m ()
multipart_ url opts = liftIO . void . runCurl . Internal.curlMultipart url opts

-- | Run a custom curl request (as specified by the 'CurlOption's), returning
-- the 'CurlReponse'
-- NOTE: This sets the 'WriteFun' and 'HeadFun' options!
runWithResponse :: MonadIO m => UrlString -> [CurlOption] -> m CurlResponse
runWithResponse url = liftIO . runCurl . Internal.runWithResponse url

-- | Run a custom curl request (as specified by the 'CurlOption's), returning
-- the 'CurlReponse'. Also includes the 'InfoValue's taken from the provided
-- 'Info's
-- NOTE: This sets the 'WriteFun' and 'HeadFun' options!
runWithResponseInfo ::
  MonadIO m => UrlString -> [CurlOption] -> [Info] -> m CurlResponse
runWithResponseInfo url opts =
  liftIO . runCurl . Internal.runWithResponseInfo url opts

-- | Run an IO action that operates on a file pointer, performing the appropriate
-- cleanup. This can be useful for options like 'WriteData', which expect a file
-- pointer
--
-- >>> path = "./data"
-- >>> mode = OpenAppend $ Just BinaryMode
-- >>> withFilePtr mode path $ \ptr -> get_ "file:///path" [ WriteData ptr ]
withFilePtr ::
  (MonadIO m, MonadMask m) => FileOpenMode -> FilePath -> (Ptr File -> m a) -> m a
withFilePtr mode path = bracket (openFile mode path) closeFile
