-------------------------------------------------------------------------------
import           Control.Applicative      ((<$>))
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad            (forM)
import           Control.Monad.ST
-------------------------------------------------------------------------------
import           Data.Char                (toUpper)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
-------------------------------------------------------------------------------
import           NLP.Snowball.Internal
import           NLP.Snowball.ST
-------------------------------------------------------------------------------
import           System.Environment       (getArgs)
import           System.FilePath          (dropExtension, takeFileName)
-------------------------------------------------------------------------------

pairs :: [a] -> [(a,a)]
pairs (x:y:zs) = (x,y) : pairs zs
pairs _ = []

main :: IO ()
main = do
    args <- getArgs
    tests <- flip mapConcurrently args $ \file -> do
      let name = dropExtension $ takeFileName file
          algorithm = read $ toUpper (head name) : tail name
      ws <- Text.words <$> Text.readFile file
      stToIO $ do
        stemmer <- new algorithm
        forM (pairs ws) $ \(word,expected) -> do
          stemmed <- inline stem stemmer word
          return $ text stemmed == expected
    let hits = length $ filter id $ concat tests
        misses = length $ filter not $ concat tests
    putStrLn $ "Hits: " ++ show hits
    putStrLn $ "Misses: " ++ show misses
