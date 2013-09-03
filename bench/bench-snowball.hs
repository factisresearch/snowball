{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-------------------------------------------------------------------------------
import           Control.Applicative      ((<$>))
-------------------------------------------------------------------------------
import           Criterion.Main           (Pure, bench, bgroup, defaultMain,
                                           nf, whnf)
-------------------------------------------------------------------------------
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
-------------------------------------------------------------------------------
import           Language.Haskell.Extract (functionExtractorMap)
-------------------------------------------------------------------------------
import           NLP.Snowball
import qualified NLP.Snowball.IO          as Stemmer
-------------------------------------------------------------------------------

main :: IO ()
main = do
    ws <- take 50000 . Text.lines <$> Text.readFile "/usr/share/dict/words"
    defaultMain
      [ bgroup "100 words"
          $(functionExtractorMap "^bench_"
             [| \name benchmark -> bench (drop 6 name) (benchmark (take 100 ws)) |])
      , bgroup "50k words"
          $(functionExtractorMap "^bench_"
             [| \name benchmark -> bench (drop 6 name) (benchmark ws) |])
      ]

bench_stem :: [Text] -> Pure
bench_stem = nf $ foldr ((:) . stem English) []

bench_fmap_stem :: [Text] -> Pure
bench_fmap_stem = nf $ fmap (stem English)

bench_stems :: [Text] -> Pure
bench_stems = nf $ stems English

bench_stems' :: [Text] -> Pure
bench_stems' = nf $ stems' English

bench_stems'_whnf :: [Text] -> Pure
bench_stems'_whnf = whnf $ stems' English

bench_stem_IO :: [Text] -> IO [Stem]
bench_stem_IO ws = do
    english <- Stemmer.new English
    mapM (Stemmer.stem english) ws
