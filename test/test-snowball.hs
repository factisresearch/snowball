{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Monad             (forM_)
-------------------------------------------------------------------------------
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
-------------------------------------------------------------------------------
import           NLP.Snowball
import qualified NLP.Snowball.IO           as Stemmer
-------------------------------------------------------------------------------
import           Test.HUnit                (Assertion, assertBool)
import           Test.QuickCheck           (Property, (==>))
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit          (testCase)
import           Test.Tasty.QuickCheck     (testProperty)
-------------------------------------------------------------------------------


prop_stem_not_null :: Algorithm -> Text -> Property
prop_stem_not_null algorithm' txt =
    not (Text.null txt) ==> not (Text.null . text $ stem algorithm' txt)

case_english_dictionary :: Assertion
case_english_dictionary = do
    ws <- Text.lines <$> Text.readFile "/usr/share/dict/words"
    english <- Stemmer.new English
    forM_ ws $ \word -> do
      stemmed <- Stemmer.stem english word
      assertBool "not null" $ not (Text.null . text $ stemmed)


-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "NLP.Snowball"
    [ testProperty "stem not null" prop_stem_not_null
    , testCase "english dictionary" case_english_dictionary
    ]
