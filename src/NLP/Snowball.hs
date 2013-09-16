{-# LANGUAGE CPP #-}

#ifdef SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
--
-- You can do everything with only 'stem', 'Algorithm' and 'stemText'.  The
-- rest is provided for heavy-duty use with varying trade-offs.
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
import qualified Control.Monad.ST.Lazy as ST_
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable
import qualified NLP.Snowball.IO as SbIO
import qualified NLP.Snowball.ST as SbST
import qualified NLP.Snowball.ST.Lazy as SbST_
import qualified System.IO.Unsafe as IO

-- | Create a shared stemmer.
new :: Algorithm -> SbIO.Stemmer
{-# NOINLINE new #-}
new = IO.unsafeDupablePerformIO . inline SbIO.new

-- | Stem a word.
--
-- >>> stem English "purity"
-- "puriti"
--
-- This uses "NLP.Snowball.IO" via 'IO.unsafeDupablePerformIO' which is
-- thought to be safe in this case because the effects are idempotent
-- (stemmers are thread-safe and re-entrant) and the result is pure (we
-- always get the same stem back given the same input algorithm and word).
-- Similarly, since stemmers are also memory-safe they may get shared
-- between calls.
stem :: Algorithm -> Text.Text -> Stem
{-# INLINABLE stem #-}
stem algorithm = IO.unsafeDupablePerformIO . SbIO.stem (new algorithm)

-- | Lazily traverse a structure and stem each word inside it.
--
-- >>> stems English $ words "referential transparency"
-- ["referenti","transpar"]
--
-- This uses "NLP.Snowball.ST.Lazy" which means a new stemmer is created
-- for each call to this function but the same stemmer is used for the
-- whole traversal and no locking is used as it isn't necessary.
stems :: (Traversable.Traversable t) => Algorithm -> t Text.Text -> t Stem
{-# INLINABLE stems #-}
stems algorithm traversable = ST_.runST $ do
    stemmer <- inline SbST_.new algorithm
    Traversable.traverse (inline SbST_.stem stemmer) traversable

-- | Strictly traverse a structure and stem each word inside it.
--
-- >>> stems' English $ words "referential transparency"
-- ["referenti","transpar"]
--
-- This uses "NLP.Snowball.ST" which means a new stemmer is created for
-- each call to this function but the same stemmer is used for the whole
-- traversal and no locking is used as it isn't necessary.
stems' :: (Traversable.Traversable t) => Algorithm -> t Text.Text -> t Stem
{-# INLINABLE stems' #-}
stems' algorithm traversable = ST.runST $ do
    stemmer <- inline SbST.new algorithm
    Traversable.traverse (inline SbST.stem stemmer) traversable

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XOverloadedStrings
-- >>> :module
-- >>> import Data.Text
-- >>> import NLP.Snowball
-- >>> import Prelude hiding (words)
-- >>> default (Text)
