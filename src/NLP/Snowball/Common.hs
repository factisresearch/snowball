{-# LANGUAGE CPP #-}

#ifdef SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: non-portable
module NLP.Snowball.Common
    ( -- * Common data types
      Algorithm(..)
    , Encoding(..)
    , Stem
    , algorithm
    , text
    ) where

import NLP.Snowball.Internal
