module Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.DeepSeq as NF
import qualified Control.Monad as Monad
import qualified Control.Parallel.Strategies as Par
import qualified Data.Text as Text
import qualified NLP.Snowball as Snowball
import qualified NLP.Snowball.IO as IO
import qualified System.IO as IO
import qualified TestData

type Test = [(Snowball.Algorithm, [(Text.Text, Text.Text)])] -> IO ()

asyncIO :: Test
asyncIO tests =
    for tests $ \(algorithm, diffs) -> do
        stemmer <- IO.new algorithm
        for diffs $ \(word, text) -> do
            stem <- IO.stem stemmer word
            return (Snowball.text stem == text)
  where
    for xs cb = do
        bs <- Async.mapConcurrently cb xs
        bs `NF.deepseq` return ()

parIO :: Test
parIO tests =
    for tests $ \(algorithm, diffs) -> do
        stemmer <- IO.new algorithm
        for diffs $ \(word, text) -> do
            stem <- IO.stem stemmer word
            return (Snowball.text stem == text)
  where
    parList = Par.withStrategy (Par.parList Par.rpar)
    for xs cb = do
        bs <- mapM cb xs
        parList bs `NF.deepseq` return ()

parStem :: Test
parStem tests = parList
    [ parList [ stem algorithm word == text | (word, text) <- diffs ]
    | (algorithm, diffs) <- tests
    ] `NF.deepseq` return ()
  where
    stem algorithm = Snowball.text . Snowball.stem algorithm
    parList = Par.withStrategy (Par.parList Par.rpar)

parStems :: Test
parStems tests = parList
    [ parList (stems algorithm word) == parList text
    | (algorithm, diffs) <- tests
    , let (word, text) = unzip diffs
    ] `NF.deepseq` return ()
  where
    stems algorithm = map Snowball.text . Snowball.stems algorithm
    parList = Par.withStrategy (Par.parList Par.rpar)

parStems' :: Test
parStems' tests = parList
    [ parList (stems algorithm word) == parList text
    | (algorithm, diffs) <- tests
    , let (word, text) = unzip diffs
    ] `NF.deepseq` return ()
  where
    stems algorithm = map Snowball.text . Snowball.stems' algorithm
    parList = Par.withStrategy (Par.parList Par.rpar)

main :: IO ()
main = do
    tests <- for (enumFrom minBound) $ \algorithm -> do
        diffs <- TestData.diffs (show algorithm)
        return (algorithm, diffs)
    let run name test = do
            putStr name
            putStr "... "
            IO.hFlush IO.stdout
            Monad.void (test tests)
            putStrLn "OK"
    run "asyncIO" asyncIO
    run "parIO" parIO
    run "parStem" parStem
    run "parStems" parStems
    run "parStems'" parStems'
    putStrLn "All OK."
  where
    for = Monad.forM
