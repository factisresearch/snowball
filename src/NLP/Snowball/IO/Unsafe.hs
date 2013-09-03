{-# LANGUAGE CPP #-}

#ifdef SAFE_HASKELL
{-# LANGUAGE Unsafe #-}
#endif

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: portable
module NLP.Snowball.IO.Unsafe
    ( -- * Mid-level unsafe ByteString interface in IO
      Stemmer
    , list
    , new
    , delete
    , with
    , stem
    , module NLP.Snowball.Common
    ) where

import NLP.Snowball.Common
import NLP.Snowball.Internal
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as ByteString
import qualified Foreign
import qualified NLP.Snowball.IO.Unsafe.C as C

-- | An instance of a stemmer in memory.  It is /not/ safe to share
-- a stemmer between threads without locks!
type Stemmer = Foreign.Ptr C.Stemmer

-- | Get a listing of available stemmer algorithms.  Included for
-- completeness; you might find the 'Bounded' and 'Enum' instances of
-- 'Algorithm' more useful.
list :: IO [ByteString.ByteString]
{-# INLINABLE list #-}
list = do
    names <- inline C.list
    array <- Foreign.peekArray0 Foreign.nullPtr names
    mapM ByteString.packCString array

-- | Create a new 'Stemmer' with the given algorithm and encoding.  The
-- stemmer is /not/ subject to garbage collection!  It is your
-- responsibility to call 'delete' when the stemmer isn't used anymore.
new :: Algorithm -> Encoding -> IO Stemmer
{-# INLINABLE new #-}
new algorithm encoding =
    ByteString.useAsCString (algorithmName algorithm) $ \algorithm' ->
    ByteString.useAsCString (encodingName encoding) $ \encoding' ->
        Foreign.throwIfNull "NLP.Snowball.Unsafe.new: nullPtr" $
            inline C.new algorithm' encoding'
  where

    algorithmName Danish = ByteString.pack "da"
    algorithmName Dutch = ByteString.pack "nl"
    algorithmName English = ByteString.pack "en"
    algorithmName Finnish = ByteString.pack "fi"
    algorithmName French = ByteString.pack "fr"
    algorithmName German = ByteString.pack "de"
    algorithmName Hungarian = ByteString.pack "hu"
    algorithmName Italian = ByteString.pack "it"
    algorithmName Norwegian = ByteString.pack "no"
    algorithmName Porter = ByteString.pack "porter"
    algorithmName Portuguese = ByteString.pack "pt"
    algorithmName Romanian = ByteString.pack "ro"
    algorithmName Russian = ByteString.pack "ru"
    algorithmName Spanish = ByteString.pack "es"
    algorithmName Swedish = ByteString.pack "sv"
    algorithmName Turkish = ByteString.pack "tr"

    encodingName ISO_8859_1 = ByteString.pack "ISO_8859_1"
    encodingName ISO_8859_2 = ByteString.pack "ISO_8859_2"
    encodingName KOI8_R = ByteString.pack "KOI8_R"
    encodingName UTF_8 = ByteString.pack "UTF_8"

-- | Delete a 'Stemmer', freeing its allocated resources.  Do /not/ use
-- a stemmer that has been deleted!
delete :: Stemmer -> IO ()
{-# INLINABLE delete #-}
delete = inline C.delete

-- | Create a new 'Stemmer' with the given algorithm and encoding, and pass
-- it to the given callback.  The stemmer will be deleted when the callback
-- returns or throws an exception.  Do /not/ return the stemmer from the
-- callback!  It has been deleted and is unusable.
with :: Algorithm -> Encoding -> (Stemmer -> IO a) -> IO a
{-# INLINABLE with #-}
with algorithm encoding =
    Exception.bracket (inline new algorithm encoding) (inline delete)

-- | Compute the stem of the given word with the given stemmer.  The word
-- needs to be encoded in the 'Encoding' the 'Stemmer' was created with, as
-- will the result be.
stem :: Stemmer -> ByteString.ByteString -> IO ByteString.ByteString
{-# INLINABLE stem #-}
stem stemmer word =
    ByteString.useAsCStringLen word $ \(word',size) -> do
        word'' <- Foreign.throwIfNull "NLP.Snowball.Unsafe.stem: nullPtr" $
            inline C.stem stemmer word' (fromIntegral size)
        length' <- inline C.length stemmer
        ByteString.packCStringLen (word'',fromIntegral length')
