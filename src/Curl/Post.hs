--------------------------------------------------------------------

--------------------------------------------------------------------

-- |
-- Module    : Curl.Post
-- Copyright : (c) Galois Inc 2007-2009
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing and marshalling formdata (as part of POST uploads\/submissions.)
-- If you are only looking to submit a sequence of name=value pairs,
-- you are better off using the CurlPostFields constructor; much simpler.
module Curl.Post where

import Control.Monad (foldM, (<=<))
import Data.Functor (($>))
import Data.Word (Word32)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (pokeByteOff, sizeOf))
import Curl.Types (Slist)

type Header = String

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
multiformString x y =
  HttpPost
    { postName = x,
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
      [] -> undefined
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
  cs2 <- mapM newCString extraHeaders
  ip <- foldM slistAppend nullPtr cs2
  pokeByteOff php (ptrIndex 8) ip
  pokeByteOff php (ptrIndex 9) nullPtr
  maybe
    (pokeByteOff php (ptrIndex 11) nullPtr)
    (pokeByteOff php (ptrIndex 11) <=< newCString)
    showName
  pure php
  where
    ptrIndex :: Int -> Int
    ptrIndex n = n * sizeOf nullPtr

foreign import ccall "curl_slist_append"
  slistAppend :: Ptr Slist -> CString -> IO (Ptr Slist)

foreign import ccall "curl_slist_free_all" curlSlistFree :: Ptr Slist -> IO ()

foreign import ccall "curl_formfree" curlFormfree :: Ptr a -> IO ()
