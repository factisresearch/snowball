import Data.List ((\\))
import NLP.Snowball.Common
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import qualified NLP.Snowball.IO.Unsafe as Unsafe
import System.IO.Error (isUserError)
import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldThrow)

main :: IO ()
main = hspec $ do
    algorithmSpec
    unsafeSpec

algorithms :: [Algorithm]
algorithms = enumFrom minBound

algorithmSpec :: Spec
algorithmSpec = describe "Algorithm" $
    it "has all algorithm_names and in the same order" $ do
        let name = ByteString.pack . map Char.toLower . show
        names <- Unsafe.list
        map name algorithms `shouldBe` names

unsafeSpec :: Spec
unsafeSpec = describe "Unsafe" $
    it "throws if the algorithm doesn't support the encoding" $ do
        mapM_ (newShouldThrow ISO_8859_1) [Romanian, Russian, Turkish]
        mapM_ (newShouldThrow ISO_8859_2) (algorithms \\ [Romanian])
        mapM_ (newShouldThrow KOI8_R) (algorithms \\ [Russian])
  where
    newShouldThrow encoding algorithm' =
        Unsafe.new algorithm' encoding `shouldThrow` isUserError
