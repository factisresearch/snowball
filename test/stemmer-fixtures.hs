{-# LANGUAGE StandaloneDeriving #-}

-------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (forM)
-------------------------------------------------------------------------------
import           Data.Char           (toUpper)
-------------------------------------------------------------------------------
import           NLP.Stemmer         (Algorithm (..), stem)
-------------------------------------------------------------------------------
import           System.Environment  (getArgs)
import           System.FilePath     (dropExtension, takeFileName)
-------------------------------------------------------------------------------


deriving instance Read Algorithm

-------------------------------------------------------------------------------

pairs :: [a] -> [(a,a)]
pairs (x:y:zs) = (x,y) : pairs zs
pairs _ = []


-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    tests <- forM args $ \file -> do
      let name = dropExtension $ takeFileName file
          algorithm = read $ (toUpper $ head name) : (tail name)
      ws <- words <$> readFile file
      forM (pairs ws) $ \(word,expected) -> do
        let stemmed = stem algorithm word
        return $ stemmed == expected
    let hits = length $ filter id $ concat tests
        misses = length $ filter not $ concat tests
    putStrLn $ "Hits: " ++ show hits
    putStrLn $ "Misses: " ++ show misses
