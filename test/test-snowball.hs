{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
-------------------------------------------------------------------------------
import           Test.HUnit                (Assertion, assertBool)
import           Test.QuickCheck           (Arbitrary (..), Property, elements,
                                            (==>))
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit          (testCase)
import           Test.Tasty.QuickCheck     (testProperty)
-------------------------------------------------------------------------------


deriving instance Bounded Algorithm
deriving instance Enum Algorithm
deriving instance Show Algorithm

instance Arbitrary Algorithm where
    arbitrary = elements [minBound ..]


-------------------------------------------------------------------------------

prop_stem_not_null :: Algorithm -> Text -> Property
prop_stem_not_null algorithm txt =
    not (Text.null txt) ==> not (Text.null $ stem algorithm txt)

case_english_dictionary :: Assertion
case_english_dictionary = do
    ws <- Text.lines <$> Text.readFile "/usr/share/dict/words"
    english <- newStemmer English
    forM_ ws $ \word -> do
      stemmed <- stemWith english word
      assertBool "not null" $ not (Text.null stemmed)


-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "NLP.Snowball"
    [ testProperty "stem not null" prop_stem_not_null
    , testCase "english dictionary" case_english_dictionary
    ]
