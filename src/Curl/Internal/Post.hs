-- FIXME
-- This entire module needs to change
-- Should be FFI bindings for `curl_mime`
module Curl.Internal.Post where

import Control.Monad (foldM, (<=<))
import Curl.Internal.Types (slistAppend)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.CaseInsensitive as CaseInsensitive
import Data.Functor (($>), (<&>))
import Data.Word (Word32)
import Foreign.C.String (newCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (pokeByteOff, sizeOf))
import Network.HTTP.Types (Header)

data HttpPost = HttpPost
  { postName :: String,
    contentType :: Maybe String,
    content :: Content,
    extraHeaders :: [Header],
    showName :: Maybe String
  }
  deriving stock (Eq, Show)

data Content
  = ContentFile FilePath
  | ContentBuffer (Ptr CChar) Word32 -- byte arrays also?
  | ContentString String
  deriving stock (Eq, Show)

multiformString :: String -> String -> HttpPost
multiformString postName y =
  HttpPost
    { postName,
      content = ContentString y,
      contentType = Nothing,
      extraHeaders = [],
      showName = Nothing
    }

-- lower-level marshalling code.

sizeofHttpPost :: Int
sizeofHttpPost = 12 * sizeOf (nullPtr @CChar)

marshallPosts :: [HttpPost] -> IO (Ptr HttpPost)
marshallPosts = \case
  [] -> pure nullPtr
  ms ->
    traverse marshallPost ms >>= \case
      [] -> pure nullPtr
      (x : xs) -> linkUp x xs $> x
  where
    linkUp :: Ptr a -> [Ptr a] -> IO ()
    linkUp p = \case
      [] -> pokeByteOff p 0 nullPtr
      (x : xs) -> pokeByteOff p 0 x *> linkUp x xs

marshallPost :: HttpPost -> IO (Ptr HttpPost)
marshallPost HttpPost {..} = do
  php <- mallocBytes sizeofHttpPost
  pokeByteOff php 0 nullPtr
  newCString postName >>= pokeByteOff php (ptrIndex 1)
  pokeByteOff php (ptrIndex 2) (length postName)
  case content of
    ContentFile f -> do
      newCString f >>= pokeByteOff php (ptrIndex 3)
      pokeByteOff php (ptrIndex 4) (length f)
      pokeByteOff php (ptrIndex 5) nullPtr
      pokeByteOff php (ptrIndex 6) nullPtr
      pokeByteOff @Word32 php (ptrIndex 10) 0x1
    ContentBuffer ptr len -> do
      pokeByteOff php (ptrIndex 3) nullPtr
      pokeByteOff php (ptrIndex 4) nullPtr
      pokeByteOff php (ptrIndex 5) ptr
      pokeByteOff php (ptrIndex 6) len
      pokeByteOff @Word32 php (ptrIndex 10) 0x10
    ContentString s -> do
      newCString s >>= pokeByteOff php (ptrIndex 3)
      pokeByteOff php (ptrIndex 4) (length s)
      pokeByteOff php (ptrIndex 5) nullPtr
      pokeByteOff php (ptrIndex 6) nullPtr
      pokeByteOff @Word32 php (ptrIndex 10) 0x4
  cs1 <- maybe (pure nullPtr) newCString contentType
  pokeByteOff php (ptrIndex 7) cs1
  cs2 <- traverse newCString strHeaders
  ip <- foldM slistAppend nullPtr cs2
  pokeByteOff php (ptrIndex 8) ip
  pokeByteOff php (ptrIndex 9) nullPtr
  maybe
    (pokeByteOff php (ptrIndex 11) nullPtr)
    (pokeByteOff php (ptrIndex 11) <=< newCString)
    showName
  pure php
  where
    strHeaders :: [String]
    strHeaders =
      extraHeaders <&> \(k, v) ->
        ByteString.Char8.unpack $
          CaseInsensitive.original k <> ": " <> v

    ptrIndex :: Int -> Int
    ptrIndex n = n * sizeOf nullPtr

foreign import ccall "curl_formfree" formFree :: Ptr a -> IO ()
