{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bindings to the Snowball library.
module Text.Snowball
    ( Algorithm(..)
    , stem
    )
  where

-------------------------------------------------------------------------------
import           Control.Exception     (finally)
import           Control.Monad         (forM)
-------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString, packCString,
                                        packCStringLen, useAsCString)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Text.Encoding    (decodeUtf8', encodeUtf8)
-------------------------------------------------------------------------------
import           Foreign               (Ptr)
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

stems :: Algorithm -> [Text] -> [Text]
stems algorithm words =
    unsafePerformIO $ withStemmer algorithm $ \stemmer ->
      forM words $ \word ->
        useAsCString (encodeUtf8 word) $ \word' ->
          do ptr <- sb_stemmer_stem stemmer word' (fromIntegral $ Text.length word)
             len <- sb_stemmer_length stemmer
             bytes <- packCStringLen (ptr,fromIntegral len)
             return $ either (const word) id $ decodeUtf8' bytes

{-# RULES "map/stem" forall a xs. map (stem a) xs = stems a xs #-}


-------------------------------------------------------------------------------

data Stemmer

foreign import ccall unsafe "libstemmer.h sb_stemmer_new"
    sb_stemmer_new :: CString -> CString -> IO (Ptr Stemmer)

foreign import ccall unsafe "libstemmer.h sb_stemmer_delete"
    sb_stemmer_delete :: Ptr Stemmer -> IO ()

foreign import ccall unsafe "libstemmer.h sb_stemmer_stem"
    sb_stemmer_stem :: Ptr Stemmer -> CString -> CInt -> IO (CString)

foreign import ccall unsafe "libstemmer.h sb_stemmer_length"
    sb_stemmer_length :: Ptr Stemmer -> IO CInt

withStemmer :: Algorithm -> (Ptr Stemmer -> IO a) -> IO a
withStemmer algorithm action =
    useAsCString (algorithmName algorithm) $ \name ->
      useAsCString "UTF_8" $ \utf8 ->
        do stemmer <- sb_stemmer_new name utf8
           action stemmer `finally` sb_stemmer_delete stemmer

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
