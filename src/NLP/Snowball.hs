-- |
-- Portability: Haskell2010 plus text
--
-- Bindings to the Snowball library.
module NLP.Snowball
    ( -- * Pure interface
      stem
    , stems
    , algorithm
    , text
      -- * IO interface
    , newStemmer
    , stemWith
    , stemsWith
      -- * Types
    , Algorithm(..)
    , Stem
    , Stemmer
    ) where

import           Control.Concurrent    (MVar, newMVar, withMVar)
import           Control.DeepSeq       (NFData)
import           Control.Monad         (when)
import qualified Data.ByteString       as ByteString
import           Data.ByteString.Char8 (ByteString, pack, packCStringLen,
                                        useAsCString)
import           Data.Text             (Text)
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Data.Traversable      (Traversable, forM)
import           Foreign               (FinalizerPtr, ForeignPtr, Ptr,
                                        newForeignPtr, nullPtr, withForeignPtr)
import           Foreign.C             (CInt (..), CString)
import           System.IO.Unsafe      (unsafePerformIO)

-- | Snowball algorithm used for stemming words.
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
    | Portuguese
    | Romanian
    | Russian
    | Spanish
    | Swedish
    | Turkish
    | Porter -- ^ Use 'English' instead.
    deriving (Eq, Ord)

instance NFData Algorithm

-- | A 'Stem' can only be created by stemming a word, and two stems are
-- only considered equal if both the 'Algorithm' used and the computed
-- stems are equal.
data Stem = Stem !Algorithm !ByteString deriving (Eq, Ord)

instance Show Stem where
    show = show . text

instance NFData Stem

-- | Get back the 'Algorithm' that was used to compute a 'Stem'.
algorithm :: Stem -> Algorithm
algorithm (Stem algorithm' _) = algorithm'

-- | Decode a computed 'Stem' into a 'Text' value.
text :: Stem -> Text
text (Stem _ bytes) = decodeUtf8 bytes

-- | Compute the 'Stem' of a word using the specified 'Algorithm'.
--
-- >>> stem English "fantastically"
-- "fantast"
stem :: Algorithm -> Text -> Stem
stem algorithm' =
    unsafePerformIO . stemWith stemmer
  where
    stemmer = unsafePerformIO $ newStemmer algorithm'

-- | Strictly map the 'stem' function over a 'Traversable' while sharing
-- a single 'Stemmer' instance and locking it only once.
stems :: (Traversable t) => Algorithm -> t Text -> t Stem
{-# INLINABLE stems #-}
stems algorithm' ws = unsafePerformIO $ do
    stemmer <- newStemmer algorithm'
    stemsWith stemmer ws

-- | A thread and memory safe Snowball stemmer instance.
data Stemmer = Stemmer !Algorithm !(MVar (ForeignPtr SbStemmer))

-- | Create a new 'Stemmer' instance for the given 'Algorithm'.
newStemmer :: Algorithm -> IO Stemmer
newStemmer algorithm' =
    useAsCString (algorithmName algorithm') $ \name -> do
        sb_stemmer <- sb_stemmer_new name nullPtr
        when (sb_stemmer == nullPtr) $ error "NLP.Snowball.newStemmer: nullPtr"
        foreignPtr <- newForeignPtr sb_stemmer_delete sb_stemmer
        mvar <- newMVar foreignPtr
        return $ Stemmer algorithm' mvar

-- | Compute the stem of a single word.  The 'Stemmer' will be locked and
-- unlocked once for each call to this function.
stemWith :: Stemmer -> Text -> IO Stem
stemWith stemmer word = do
    [a] <- stemsWith stemmer [word]
    return a

-- | Compute stems for every word in a 'Traversable'.  The 'Stemmer' will
-- be locked and unlocked once for each call to this function, independent
-- of the number of words getting stemmed.
stemsWith :: (Traversable t) => Stemmer -> t Text -> IO (t Stem)
{-# INLINABLE stemsWith #-}
stemsWith (Stemmer algorithm' mvar) ws =
    withMVar mvar $ \foreignPtr ->
    withForeignPtr foreignPtr $ \sb_stemmer ->
    forM ws $ \word -> do
        let word' = encodeUtf8 word
        useAsCString word' $ \word'' -> do
            let size = fromIntegral $ ByteString.length word'
            ptr <- sb_stemmer_stem sb_stemmer word'' size
            len <- sb_stemmer_length sb_stemmer
            bytes <- packCStringLen (ptr,fromIntegral len)
            return $ Stem algorithm' bytes

data SbStemmer

foreign import ccall unsafe "sb_stemmer_new"
    sb_stemmer_new :: CString -> CString -> IO (Ptr SbStemmer)

foreign import ccall unsafe "&sb_stemmer_delete"
    sb_stemmer_delete :: FinalizerPtr SbStemmer

foreign import ccall unsafe "sb_stemmer_stem"
    sb_stemmer_stem :: Ptr SbStemmer -> CString -> CInt -> IO CString

foreign import ccall unsafe "sb_stemmer_length"
    sb_stemmer_length :: Ptr SbStemmer -> IO CInt

algorithmName :: Algorithm -> ByteString
algorithmName Danish     = pack "da"
algorithmName Dutch      = pack "nl"
algorithmName English    = pack "en"
algorithmName Finnish    = pack "fi"
algorithmName French     = pack "fr"
algorithmName German     = pack "de"
algorithmName Hungarian  = pack "hu"
algorithmName Italian    = pack "it"
algorithmName Norwegian  = pack "no"
algorithmName Portuguese = pack "pt"
algorithmName Romanian   = pack "ro"
algorithmName Russian    = pack "ru"
algorithmName Spanish    = pack "es"
algorithmName Swedish    = pack "sv"
algorithmName Turkish    = pack "tr"
algorithmName Porter     = pack "porter"
