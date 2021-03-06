{-# LANGUAGE CPP #-}

#ifdef SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
--
-- You can do everything with only 'stem', 'Algorithm' and 'text'.  The rest is
-- provided for heavy-duty use with varying trade-offs.
--
-- ["NLP.Snowball"] Provides simplicity in the form of pure wrappers around
-- the other interfaces.
--
-- ["NLP.Snowball.IO"] Provides space-efficiency by allowing stemmers to be
-- shared safely, even between threads.
--
-- ["NLP.Snowball.ST" and "NLP.Snowball.ST.Lazy"] Provide time-efficiency for
-- bulk operations by restricting stemmers to single-threaded use, thus
-- avoiding the need for locks.
--
-- ["NLP.Snowball.IO.Unsafe"] May provide even better space and time
-- efficiency than the other interfaces, but at the expense of safety.
module NLP.Snowball
    ( -- * High-level safe Text interface
      stem
    , stems
    , stems'
    , module NLP.Snowball.Common
    ) where

import NLP.Snowball.Common
import NLP.Snowball.Internal
import qualified Control.Monad.ST as ST
import qualified Control.Monad.ST.Lazy as ST.Lazy
import qualified Data.Text as Text
import qualified Data.Traversable as Iter
import qualified NLP.Snowball.IO as IO
import qualified NLP.Snowball.ST as ST
import qualified NLP.Snowball.ST.Lazy as ST.Lazy
import qualified System.IO.Unsafe as IO

-- | Create a shared stemmer.
new :: Algorithm -> IO.Stemmer
{-# NOINLINE new #-}
new = IO.unsafePerformIO . IO.new

-- | Stem a word.
--
-- >>> stem English "purity"
-- "puriti"
stem :: Algorithm -> Text.Text -> Stem
{-# INLINABLE stem #-}
stem algorithm' = IO.unsafePerformIO . IO.stem (new algorithm')

-- | Lazily traverse a structure and stem each word inside it.
--
-- >>> stems English $ words "referential transparency"
-- ["referenti","transpar"]
--
-- This uses "NLP.Snowball.ST.Lazy" which means a new stemmer is created
-- for each call to this function but the same stemmer is used for the
-- whole traversal and no locking is used as it isn't necessary.
stems :: (Iter.Traversable t) => Algorithm -> t Text.Text -> t Stem
{-# INLINABLE stems #-}
stems algorithm' iter = ST.Lazy.runST $ do
    stemmer <- inline ST.Lazy.new algorithm'
    Iter.traverse (inline ST.Lazy.stem stemmer) iter

-- | Strictly traverse a structure and stem each word inside it.
--
-- >>> stems' English $ words "referential transparency"
-- ["referenti","transpar"]
--
-- This uses "NLP.Snowball.ST" which means a new stemmer is created for
-- each call to this function but the same stemmer is used for the whole
-- traversal and no locking is used as it isn't necessary.
stems' :: (Iter.Traversable t) => Algorithm -> t Text.Text -> t Stem
{-# INLINABLE stems' #-}
stems' algorithm' iter = ST.runST $ do
    stemmer <- inline ST.new algorithm'
    Iter.traverse (inline ST.stem stemmer) iter

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XOverloadedStrings
-- >>> :module
-- >>> import Data.Text
-- >>> import NLP.Snowball
-- >>> import Prelude hiding (words)
-- >>> default (Text)
