module TestData where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.FilePath as FilePath

lowerCase :: String -> String
lowerCase (c:cs) = Char.toLower c : cs
lowerCase _ = []

directory :: FilePath
directory = FilePath.joinPath
    [ "lib"
    , "snowball_all"
    , "algorithms"
    ]

file :: FilePath -> String -> FilePath
file name algorithm = FilePath.joinPath
    [ directory
    , lowerCase algorithm
    , name
    ]

readLines :: FilePath -> IO [Text.Text]
readLines = fmap (Text.lines . Text.decodeUtf8) . ByteString.readFile

voc :: String -> IO [Text.Text]
voc = readLines . file "voc.txt"

output :: String -> IO [Text.Text]
output = readLines . file "output.txt"

diffs :: String -> IO [(Text.Text, Text.Text)]
diffs algorithm = do
    voc' <- voc algorithm
    output' <- output algorithm
    return (zip voc' output')
