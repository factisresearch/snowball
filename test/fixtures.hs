{-# LANGUAGE StandaloneDeriving #-}

-------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_, when)
-------------------------------------------------------------------------------
import           Data.Char           (toUpper)
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
-------------------------------------------------------------------------------
import           NLP.Snowball        (Algorithm (..), newStemmer, stemWith)
-------------------------------------------------------------------------------
import           System.Environment  (getArgs)
import           System.FilePath     (dropExtension, takeFileName)
-------------------------------------------------------------------------------


deriving instance Read Algorithm
deriving instance Show Algorithm

-------------------------------------------------------------------------------

main = do
    args <- getArgs
    forM_ args $ \file -> do
      let name = dropExtension $ takeFileName file
          algorithm = read $ (toUpper $ head name) : (tail name)
      stemmer <- newStemmer algorithm
      ls <- Text.lines <$> Text.readFile file
      forM_ ls $ \line -> do
        let [word,expected] = Text.words line
        stemmed <- stemWith stemmer word
        when (stemmed /= expected) $
          error . concat $ ["stem ", show algorithm, " ", Text.unpack word,
                            " == ", Text.unpack stemmed,
                            " /= ", Text.unpack expected]
