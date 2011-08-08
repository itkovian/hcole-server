{-# LANGUAGE MultiParamTypeClasses #-}
{-|

Typeclass for a cache for sequences that have been run by COLE.

-}

module Snap.Extension.Cache (
    -- * Cache typeclass 
    Cache(..)
  ) where

import Data.Maybe

class Cache a b where
  -- Check to see if a key is present in the cache. If it is, return the associated data, if not Nothing
  request :: a -> String -> Maybe b
  insert :: a -> String -> a


