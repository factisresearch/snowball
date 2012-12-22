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

pairs :: [a] -> [(a,a)]
pairs (x:y:zs) = (x,y) : pairs zs
pairs _ = []


-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \file -> do
      let name = dropExtension $ takeFileName file
          algorithm = read $ (toUpper $ head name) : (tail name)
      stemmer <- newStemmer algorithm
      ws <- Text.words <$> Text.readFile file
      forM_ (pairs ws) $ \(word,expected) -> do
        stemmed <- stemWith stemmer word
        when (stemmed /= expected) $
          error . concat $ ["stem ", show algorithm, " ", show $ Text.unpack word,
                            " == ", show $ Text.unpack stemmed,
                            " /= ", show $ Text.unpack expected]
