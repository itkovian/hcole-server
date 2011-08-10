{-

This module represents the files in the cache of the COLE environment. It offers
functions to access this data.

-}


module Cole.Files.Cache
  ( getSpeedup
  , getCompilationTime
  , getCodeSize
  , mkCacheFile
  ) where

import qualified Codec.Archive.Tar as T
import qualified Codec.Archive.Tar.Entry as TE
import qualified Codec.Compression.GZip as GZ
import           Control.Monad (mplus)
import qualified Data.ByteString.Lazy as BS
import           Data.List (isSuffixOf)

data CacheFile = CacheFile 
    { cacheFilePath :: FilePath 
    , cacheFileEntries :: T.Entries
    }

--------------------------------------------------------------------------------
-- | Locate an entry in the tarball
findEntry :: FilePath -> T.Entries -> Maybe TE.Entry
findEntry path = T.foldEntries (\e m -> m `mplus` toMaybe e) Nothing error
  where
    toMaybe e = if isSuffixOf path (T.entryPath e) then Just e else Nothing


--------------------------------------------------------------------------------
-- | Turn the tarball into a CacheFile structure
mkCacheFile :: FilePath -> IO CacheFile
mkCacheFile fp = do
  content <- BS.readFile fp
  let entries = T.read $ GZ.decompress content
  return $ CacheFile { cacheFilePath = fp
                     , cacheFileEntries = entries } 

--------------------------------------------------------------------------------
-- | Get the speedup data from the cached file
getSpeedup :: CacheFile -> Maybe BS.ByteString
getSpeedup cf = do
    e <- findEntry "exec_times_ref.csv" (cacheFileEntries cf)
    case TE.entryContent e of 
      TE.NormalFile bs _ -> Just bs
      _                  -> Nothing

--------------------------------------------------------------------------------
-- | Get the compilation time data from the cached file
getCompilationTime = undefined


--------------------------------------------------------------------------------
-- | Get the code size data from the cached file 
getCodeSize = undefined

