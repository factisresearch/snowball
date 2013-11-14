{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

#ifdef SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
module NLP.Snowball.IO
    ( -- * High-level safe Text interface in IO
      Stemmer
    , new
    , stem
    , module NLP.Snowball.Common
    ) where

import NLP.Snowball.Common
import NLP.Snowball.Internal
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Data.Data as Data
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Foreign
import qualified NLP.Snowball.IO.Unsafe as Unsafe
import qualified NLP.Snowball.IO.Unsafe.C as C

-- | A thread and memory safe Snowball stemmer.
data Stemmer = Stemmer
    !Algorithm
    !(Concurrent.MVar (Foreign.ForeignPtr C.Stemmer))
  deriving Data.Typeable

-- | Create a new 'Stemmer' instance using the given 'Algorithm'.
new :: Algorithm -> IO Stemmer
{-# INLINABLE new #-}
new algorithm' = Exception.bracketOnError
    (inline Unsafe.new algorithm' UTF_8)
    (inline Unsafe.delete)
    (\stemmer -> do
        fptr <- Foreign.newForeignPtr (inline C.finalize) stemmer
        fmap (Stemmer algorithm') (Concurrent.newMVar fptr))

-- | Stem a word.
--
-- >>> let paper = "Imperative functional programming"
-- >>> english <- new English
-- >>> mapM (stem english) $ words paper
-- ["Imperat","function","program"]
stem :: Stemmer -> Text.Text -> IO Stem
{-# INLINABLE stem #-}
stem (Stemmer algorithm' mvar) word =
    Concurrent.withMVar mvar $ \fptr ->
    Foreign.withForeignPtr fptr $ \stemmer -> fmap
        (inline mkStem algorithm')
        (inline Unsafe.stem stemmer (Text.encodeUtf8 word))

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XOverloadedStrings
-- >>> :module
-- >>> import Data.Text
-- >>> import NLP.Snowball.IO
-- >>> import Prelude hiding (words)
-- >>> default (Text)
