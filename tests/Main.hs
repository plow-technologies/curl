module Main (main) where

import qualified Curl
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import System.FilePath ((</>))
import qualified Test.Hspec as Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

main :: IO ()
main = Hspec.hspec $ do
  getSpec

getSpec :: Hspec.Spec
getSpec = Hspec.describe "GET request"
  . Hspec.it "Gets a file"
  . withSystemTempDirectory "curl-tests"
  $ \dir -> do
    let fp :: FilePath
        fp = dir </> "test-file"

        contents :: ByteString
        contents = ByteString.Char8.replicate 100 'a'

    ByteString.Char8.writeFile fp contents
    url <- Curl.mkUrl $ "file:" <> ByteString.Char8.pack fp
    (code, body) <- Curl.get url mempty
    code `Hspec.shouldBe` Curl.CurlOK
    ByteString.Lazy.toStrict body `Hspec.shouldBe` contents
