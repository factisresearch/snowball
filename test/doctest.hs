import System.Directory (makeRelativeToCurrentDirectory)
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
    sources <- glob "src/**/*.hs"
    relative <- mapM makeRelativeToCurrentDirectory sources
    doctest $ filter (`notElem` blacklist) relative
  where
    blacklist = ["src/NLP/Snowball/IO/Unsafe/C.hs"]
