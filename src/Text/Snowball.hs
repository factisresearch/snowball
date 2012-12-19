{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bindings to the Snowball library.
module Text.Snowball
    ( -- * Pure interface
      Algorithm(..)
    , stem
    , stems
      -- * IO interface
    , Stemmer
    , newStemmer
    , stemIO
    , stemsIO
    )
  where

-------------------------------------------------------------------------------
import           Control.Concurrent    (MVar, newMVar, withMVar)
import           Control.Monad         (forM)
-------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString, packCStringLen,
                                        useAsCString)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Text.Encoding    (decodeUtf8', encodeUtf8)
-------------------------------------------------------------------------------
import           Foreign               (ForeignPtr, FunPtr, Ptr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.C             (CInt (..), CString)
-------------------------------------------------------------------------------
import           System.IO.Unsafe      (unsafePerformIO)
-------------------------------------------------------------------------------


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
--   >>> stem English "fantastically"
--   "fantast"
stem :: Algorithm -> Text -> Text
stem algorithm word = let [a] = stems algorithm [word] in a

-- | Compute the stems of several words in one go.  This can be more
--   efficient than @'map' 'stem'@ because it uses a single 'Stemmer'
--   instance, however the @map@ version is rewritten to use this function
--   with a rewrite rule.  You can still use this function though if you
--   want to make sure it is used or if you find it more convenient.
stems :: Algorithm -> [Text] -> [Text]
stems algorithm ws =
    unsafePerformIO $
      do stemmer <- newStemmer algorithm
         stemsIO stemmer ws

{-# RULES "map/stem" forall a xs. map (stem a) xs = stems a xs #-}


-------------------------------------------------------------------------------

-- | A thread and memory safe Snowball stemmer instance.
newtype Stemmer = Stemmer (MVar (ForeignPtr Struct))

-- | Create a new reusable 'Stemmer' instance.
newStemmer :: Algorithm  -> IO Stemmer
newStemmer algorithm =
    useAsCString (algorithmName algorithm) $ \name ->
      useAsCString "UTF_8" $ \utf8 ->
        do struct <- sb_stemmer_new name utf8
           structPtr <- newForeignPtr sb_stemmer_delete struct
           mvar <- newMVar structPtr
           return $ Stemmer mvar

-- | Use a 'Stemmer' to stem a word.  This can be used more efficiently
--   than 'stem' because you can keep a stemmer around and reuse it, but it
--   requires 'IO' to ensure thread safety.
--
--   In my benchmarks, this (and 'stemsIO') is faster than 'stem' for a few
--   hundred words, but slower for larger number of words.  I don't know if
--   this is a problem with my benchmarks, with these bindings or with the
--   Snowball library itself, but make sure to benchmark yourself if speed
--   is a concern, and consider caching stems with e.g. a @HashMap@.
stemIO :: Stemmer -> Text -> IO Text
stemIO stemmer word = do
    [a] <- stemsIO stemmer [word]
    return a

-- | Use a 'Stemmer' to stem multiple words in one go.  This can be more
--   efficient than @'mapM' 'stemIO'@ because the 'Stemmer' is only locked
--   once.
stemsIO :: Stemmer -> [Text] -> IO [Text]
stemsIO (Stemmer mvar) ws =
    withMVar mvar $ \structPtr ->
      withForeignPtr structPtr $ \struct ->
        forM ws $ \word ->
          useAsCString (encodeUtf8 word) $ \word' ->
            do ptr <- sb_stemmer_stem struct word' $
                        fromIntegral $ Text.length word
               len <- sb_stemmer_length struct
               bytes <- packCStringLen (ptr,fromIntegral len)
               return $ either (const word) id $ decodeUtf8' bytes

-------------------------------------------------------------------------------

data Struct

foreign import ccall unsafe "libstemmer.h sb_stemmer_new"
    sb_stemmer_new :: CString -> CString -> IO (Ptr Struct)

foreign import ccall unsafe "libstemmer.h &sb_stemmer_delete"
    sb_stemmer_delete :: FunPtr (Ptr Struct -> IO ())

foreign import ccall unsafe "libstemmer.h sb_stemmer_stem"
    sb_stemmer_stem :: Ptr Struct -> CString -> CInt -> IO (CString)

foreign import ccall unsafe "libstemmer.h sb_stemmer_length"
    sb_stemmer_length :: Ptr Struct -> IO CInt

algorithmName :: Algorithm -> ByteString
algorithmName algorithm =
    case algorithm of
      Danish     -> "da"
      Dutch      -> "nl"
      English    -> "en"
      Finnish    -> "fi"
      French     -> "fr"
      German     -> "de"
      Hungarian  -> "hu"
      Italian    -> "it"
      Norwegian  -> "no"
      Portuguese -> "pt"
      Romanian   -> "ro"
      Russian    -> "ru"
      Spanish    -> "es"
      Swedish    -> "sv"
      Turkish    -> "tr"
      Porter     -> "porter"
