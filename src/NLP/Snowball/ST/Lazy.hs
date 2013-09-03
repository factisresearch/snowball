{-# LANGUAGE CPP #-}

#ifdef SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
module NLP.Snowball.ST.Lazy
    ( -- * High-level safe Text interface in lazy ST
      Stemmer
    , new
    , stem
    , module NLP.Snowball.Common
    ) where

import NLP.Snowball.Common
import NLP.Snowball.Internal
import qualified Control.Exception as Exception
import qualified Control.Monad.ST.Lazy.Safe as ST
import qualified Control.Monad.ST.Lazy.Unsafe as ST
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Foreign
import qualified NLP.Snowball.IO.Unsafe as Unsafe
import qualified NLP.Snowball.IO.Unsafe.C as C

-- | A memory safe Snowball stemmer encapsulated inside an 'ST.ST'
-- computation.
data Stemmer s = Stemmer !Algorithm !(Foreign.ForeignPtr C.Stemmer)

-- | Create a new 'Stemmer' instance using the given 'Algorithm'.
new :: Algorithm -> ST.ST s (Stemmer s)
{-# INLINABLE new #-}
new algorithm = ST.unsafeIOToST $ Exception.bracketOnError
    (inline Unsafe.new algorithm UTF_8)
    (inline Unsafe.delete)
    (fmap (Stemmer algorithm) . Foreign.newForeignPtr (inline C.finalize))

-- | Stem a single word.
--
-- >>> runST $ do english <- new English; stem english "fantastically"
-- "fantast"
stem :: Stemmer s -> Text.Text -> ST.ST s Stem
{-# INLINABLE stem #-}
stem (Stemmer algorithm fptr) word = ST.unsafeIOToST $
    Foreign.withForeignPtr fptr $ \stemmer -> fmap
        (Stem algorithm)
        (inline Unsafe.stem stemmer (Text.encodeUtf8 word))
