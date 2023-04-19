module Curl.Internal.Mime where

import Control.Monad (foldM, void)
import Curl.Internal.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString.Char8
import Data.Foldable (for_, traverse_)
import Data.Functor (($>))
import Foreign.C (CInt (CInt), CSize (CSize), CString, withCString)
import Foreign.Ptr (Ptr, nullPtr)
import GHC.Generics (Generic)
import Network.HTTP.Types (Header)

data MimeData
  = -- | The data to send with the part. Will be converted to a 'CString'
    -- internally (and freed automatically). Note that this will use the
    -- the length of the data passed (with a 'CStringLen') instead of using
    -- @CURL_ZERO_TERMINATED@
    MimeData ByteString
  | -- | A filepath to read the data from, which will be streamed internally
    -- by libcurl
    MimeFileData FilePath
  deriving stock (Show, Eq, Generic)

data MimePart = MimePart
  { -- | The data to send with the part, either directly or read by libcurl
    -- from a filepath
    mdata :: MimeData,
    -- | The name of the form part
    name :: Maybe String,
    -- | The remote file name
    filename :: Maybe FilePath,
    -- | Extra headers to set for this particular form part
    headers :: [Header],
    -- | The mime type
    mtype :: Maybe String,
    -- | Sets the parts data to be encoded by a specific scheme before
    -- transmission
    encoder :: Maybe MimeEncoder
  }
  deriving stock (Show, Eq, Generic)

data MimeEncoder
  = Binary
  | EightBit
  | SevenBit
  | Base64
  | QuotedPrintable
  deriving stock (Show, Eq, Generic)

newMime :: CurlHandle -> [MimePart] -> IO (Ptr Mime)
newMime curl parts =
  mimeInit curl >>= \mime -> traverse_ (addMimeParts mime) parts $> mime
  where
    addMimeParts :: Ptr Mime -> MimePart -> IO ()
    addMimeParts mime part = addPart mime >>= addMimePart part

addMimePart :: MimePart -> Ptr Part -> IO ()
addMimePart MimePart {..} part = do
  addMimeData mdata
  for_ name $ \n -> withCString n $ mimeName part
  for_ filename $ \n -> withCString n $ mimeFilename part
  for_ mtype $ \n -> withCString n $ mimeType part
  for_ encoder $ \enc ->
    ByteString.Char8.useAsCString (renderMimeEncoder enc) $
      mimeEncoder part
  addMimeHeaders headers
  where
    addMimeData :: MimeData -> IO ()
    addMimeData = \case
      MimeData bs ->
        -- Curl copies the data, so the `CString` can be freed
        void . ByteString.Char8.useAsCStringLen bs $
          uncurry (mimeData part) . fmap fromIntegral
      MimeFileData fp -> void . withCString fp $ mimeFiledata part

    addMimeHeaders :: [Header] -> IO ()
    addMimeHeaders hs =
      foldM addHeader nullPtr hs >>= \slist ->
        -- Setting the last argument to a non-zero integer will make libcurl
        -- take ownership of the `Slist`, meaning we don't need to free it
        void $ mimeHeaders part slist 1
      where
        addHeader :: Ptr Slist -> Header -> IO (Ptr Slist)
        addHeader slist header =
          -- `curl_slist_append` copies the string, so we don't need to worry
          -- about keeping it around
          ByteString.Char8.useAsCString (renderHeader header) $ slistAppend slist

renderMimeEncoder :: MimeEncoder -> ByteString
renderMimeEncoder = \case
  Binary -> "binary"
  EightBit -> "8bit"
  SevenBit -> "7bit"
  Base64 -> "base64"
  QuotedPrintable -> "quoted-printable"

data Mime

data Part

foreign import ccall "curl_mime_init" mimeInit :: CurlHandle -> IO (Ptr Mime)

foreign import ccall "curl_mime_free" mimeFree :: Ptr Mime -> IO ()

foreign import ccall "curl_mime_addpart" addPart :: Ptr Mime -> IO (Ptr Part)

foreign import ccall "curl_mime_data"
  mimeData ::
    Ptr Part -> CString -> CSize -> IO CInt

foreign import ccall "curl_mime_name" mimeName :: Ptr Part -> CString -> IO CInt

foreign import ccall "curl_mime_filedata"
  mimeFiledata :: Ptr Part -> CString -> IO CInt

foreign import ccall "curl_mime_filename"
  mimeFilename :: Ptr Part -> CString -> IO CInt

foreign import ccall "curl_mime_type" mimeType :: Ptr Part -> CString -> IO CInt

foreign import ccall "curl_mime_headers"
  mimeHeaders :: Ptr Part -> Ptr Slist -> CInt -> IO CInt

foreign import ccall "curl_mime_encoder"
  mimeEncoder ::
    Ptr Part -> CString -> IO CInt
