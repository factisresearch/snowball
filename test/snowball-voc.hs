{-# LANGUAGE CPP #-}

#ifdef STEMMER
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

module Main where

import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.FilePath as FilePath

#ifdef STEMMER
import qualified NLP.Stemmer as Stemmer
type Algorithm = Stemmer.Stemmer
deriving instance Bounded Algorithm
deriving instance Enum Algorithm
deriving instance Eq Algorithm
#else
import NLP.Snowball (Algorithm)
import qualified NLP.Snowball as Stemmer
#endif

algorithms :: [Algorithm]
algorithms = enumFrom minBound

lowerCase :: String -> String
lowerCase (c:cs) = Char.toLower c : cs
lowerCase _ = []

directory :: FilePath
directory = FilePath.joinPath
    [ "lib"
    , "snowball_all"
    , "algorithms"
    ]

file :: Algorithm -> FilePath -> FilePath
file algorithm name = FilePath.joinPath
    [ directory
    , lowerCase (show algorithm)
    , name
    ]

readLines :: FilePath -> IO [Text.Text]
readLines = fmap (Text.lines . Text.decodeUtf8) . ByteString.readFile

stems :: Algorithm -> [Text.Text] -> [Text.Text]
#ifdef STEMMER
stems algorithm = map (Text.pack . Stemmer.stem algorithm . Text.unpack)
#else
stems algorithm = map Stemmer.text . Stemmer.stems algorithm
#endif

data Result = Result { total, failures :: !Int } deriving (Show)

instance Monoid.Monoid Result where
    mempty = Result 0 0
    mappend (Result a b) (Result a' b') = Result (a + a') (b + b')

test :: Algorithm -> IO Result
test algorithm = do
    voc <- readLines (file algorithm "voc.txt")
    output <- readLines (file algorithm "output.txt")
    let stemmed = stems algorithm voc
        tests = zipWith (==) stemmed output
    return $! Result (length voc) ((length . filter not) tests)

main :: IO ()
main = print . Foldable.fold =<< Async.mapConcurrently test algorithms
