{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#ifdef STEMMER
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

module Main where

import qualified Control.Concurrent.Async as Async
import qualified Data.Foldable as Foldable
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified GHC.IO.Encoding as GHC
import qualified System.IO.Error as IO
import qualified System.Microtimer as Microtimer
import qualified TestData
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<>), (<+>))

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
    voc <- TestData.voc (show algorithm)
    output <- TestData.output (show algorithm)
    let stemmed = stems algorithm voc
        tests = zipWith (==) stemmed output
    return $! Result (length voc) ((length . filter not) tests)

run :: PP.Doc -> GHC.TextEncoding -> IO ()
run name encoding = do
    say "Package" pkg
    say "Encoding" name
    GHC.setForeignEncoding encoding
    result <- (IO.tryIOError . Microtimer.time . fmap Foldable.fold)
        (Async.mapConcurrently test algorithms)
    case result of
        Left e -> say "Error" ((PP.red . PP.text . show) e)
        Right (time, Result t f) -> do
            let fails = round (fromIntegral f / fromIntegral t * 100 :: Double)
                err = if fails > 0 then PP.red else PP.green
                secs = Microtimer.formatSeconds time
            say "Total" (PP.int t)
            say "Failed" (err (PP.int fails <> "%"))
            say "Time" (PP.text secs)
    PP.putDoc PP.line
  where
    say n d = PP.putDoc (PP.fill 9 n <> ":" <+> PP.bold d <> PP.line)
#ifdef STEMMER
    pkg = "stemmer"
#else
    pkg = "snowball"
#endif

main :: IO ()
main = do
    locale <- GHC.getForeignEncoding
    run "latin1" GHC.latin1
    run "utf8" GHC.utf8
    run "utf8_bom" GHC.utf8_bom
    run "char8" GHC.char8
    run "UTF-8//IGNORE" =<< GHC.mkTextEncoding "UTF-8//IGNORE"
    run "UTF-8//TRANSLIT" =<< GHC.mkTextEncoding "UTF-8//TRANSLIT"
    run ("initForeignEncoding" <+> (PP.parens . PP.text . show) locale) locale
