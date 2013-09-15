{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE CPP #-}

#ifdef SAFE_HASKELL
{-# LANGUAGE Unsafe #-}
#endif

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: internal
-- Portability: non-portable
--
-- Unsafe and unstable internals.  Can be used to break the safety of the
-- otherwise safe interfaces, but may be necessary if you need to write
-- instances for types in this package.
module NLP.Snowball.Internal where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
#ifdef __GLASGOW_HASKELL__
import qualified GHC.Exts as GHC
#endif

-- | Inline a call on GHC; otherwise simply the identity function.
inline :: a -> a
#ifdef __GLASGOW_HASKELL__
inline = GHC.inline
#else
inline = id
#endif

-- | Snowball algorithm defining the rules for stemming words.  The legacy
-- algorithm 'Porter' is included for completeness and may be used when
-- backwards-compatibility necessitates it, but 'English' is generally
-- preferred and superior.
data Algorithm
    = Danish
    | Dutch
    | English
    | Finnish
    | French
    | German
    | Hungarian
    | Italian
    | Norwegian
    | Porter
    | Portuguese
    | Romanian
    | Russian
    | Spanish
    | Swedish
    | Turkish

  deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Read
    , Show
    )

instance DeepSeq.NFData Algorithm

-- | Text encodings that @libstemmer@ supports.  Only 'UTF_8' is supported
-- by every 'Algorithm'.
data Encoding
    = ISO_8859_1  -- ^ Not 'Romanian', 'Russian' or 'Turkish'
    | ISO_8859_2  -- ^ Only 'Romanian'
    | KOI8_R  -- ^ Only 'Russian'
    | UTF_8

  deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Read
    , Show
    )

instance DeepSeq.NFData Encoding

-- | A 'Stem' can only be created by stemming a word, and two stems are only
-- considered equal if both the 'Algorithm' used and the computed stems are
-- equal.  This makes 'Stem' suitable for use with ordered containers like
-- @Map@ and @Set@ without us also needing to keep track of the stemming
-- algorithms, while also helping to prevent some forms of logic errors that
-- may arise when we use the same type (like 'Text.Text') for words and stems.
data Stem = Stem
    !Algorithm
    !ByteString.ByteString

  deriving
    ( Eq
    , Ord
    )

instance Show Stem where
    show = show . stemText

instance DeepSeq.NFData Stem

-- | Get back the 'Algorithm' that was used to compute a 'Stem'.
stemAlgorithm :: Stem -> Algorithm
stemAlgorithm (Stem algorithm _) = algorithm

-- | Decode a computed 'Stem' into a 'Text.Text' value.
stemText :: Stem -> Text.Text
stemText (Stem _ b) = Text.decodeUtf8 b
