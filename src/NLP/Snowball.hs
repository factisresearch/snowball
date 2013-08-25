-- |
-- Portability: Haskell2010
--
-- Bindings to the Snowball library.
module NLP.Snowball
    ( -- * Pure interface
      Algorithm(..)
    , stem
    , stems
    , stems'
      -- * IO interface
    , Stemmer
    , newStemmer
    , stemWith
    , stemsWith
    ) where

import           Control.Concurrent    (MVar, newMVar, withMVar)
import           Control.Monad         (when)
import qualified Data.ByteString       as ByteString
import           Data.ByteString.Char8 (ByteString, pack, packCStringLen,
                                        useAsCString)
import           Data.Text             (Text)
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Data.Traversable      (Traversable, forM)
import           Foreign               (ForeignPtr, FunPtr, Ptr, newForeignPtr,
                                        nullPtr, withForeignPtr)
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

-- | Compute the stem of a word using the specified algorithm.
--
-- >>> stem English "fantastically"
-- "fantast"
stem :: Algorithm -> Text -> Text
stem algorithm word = let [a] = stems algorithm [word] in a

-- | Lazily map the 'stem' function over a 'Functor' while sharing
-- a single 'Stemmer' instance.  The stemmer will be locked once for each
-- word being stemmed, and discarded once every word has been stemmed or
-- the result is discarded.
stems :: (Functor f) => Algorithm -> f Text -> f Text
stems algorithm =
    fmap (unsafePerformIO . stemWith stemmer)
  where
    stemmer = unsafePerformIO $ newStemmer algorithm

{-# RULES "map/stem" forall a. map (stem a) = stems a #-}
{-# RULES "fmap/stem" forall a. fmap (stem a) = stems a #-}

-- | Strictly map the 'stem' function over a 'Traversable' while sharing
-- a single 'Stemmer' instance and locking it only once.  The stemmer is
-- immediately discarded as garbage to be collected.
stems' :: (Traversable t) => Algorithm -> t Text -> t Text
stems' algorithm ws = unsafePerformIO $ do
    stemmer <- newStemmer algorithm
    stemsWith stemmer ws

-- | A thread and memory safe Snowball stemmer instance.
newtype Stemmer = Stemmer (MVar (ForeignPtr SbStemmer))

-- | Create a new reusable 'Stemmer' instance.
newStemmer :: Algorithm -> IO Stemmer
newStemmer algorithm =
    useAsCString (algorithmName algorithm) $ \name ->
    useAsCString (pack "UTF_8") $ \utf8 -> do
        sb_stemmer <- sb_stemmer_new name utf8
        when (sb_stemmer == nullPtr) $ error "NLP.Snowball.newStemmer: nullPtr"
        foreignPtr <- newForeignPtr sb_stemmer_delete sb_stemmer
        mvar <- newMVar foreignPtr
        return $ Stemmer mvar

-- | Use a 'Stemmer' to stem a word.  This can be used more efficiently
-- than 'stem' because you can keep a stemmer around and reuse it, but it
-- requires 'IO' to ensure thread safety.
stemWith :: Stemmer -> Text -> IO Text
stemWith stemmer word = do
    [a] <- stemsWith stemmer [word]
    return a

-- | Use a 'Stemmer' to stem multiple words in one go.  This can be more
-- efficient than @'mapM' 'stemWith'@ because the 'Stemmer' is only
-- locked once.
stemsWith :: (Traversable t) => Stemmer -> t Text -> IO (t Text)
stemsWith (Stemmer mvar) ws =
    withMVar mvar $ \foreignPtr ->
    withForeignPtr foreignPtr $ \sb_stemmer ->
    forM ws $ \word -> do
        let word' = encodeUtf8 word
        useAsCString word' $ \word'' -> do
            let size = fromIntegral $ ByteString.length word'
            ptr <- sb_stemmer_stem sb_stemmer word'' size
            len <- sb_stemmer_length sb_stemmer
            bytes <- packCStringLen (ptr,fromIntegral len)
            return $ decodeUtf8 bytes

data SbStemmer

foreign import ccall unsafe "sb_stemmer_new"
    sb_stemmer_new :: CString -> CString -> IO (Ptr SbStemmer)

foreign import ccall unsafe "&sb_stemmer_delete"
    sb_stemmer_delete :: FunPtr (Ptr SbStemmer -> IO ())

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
