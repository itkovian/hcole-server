{-

This module represents the files in the cache of the COLE environment. It offers
functions to access this data.

-}


module Cole.Files.Cache
  ( getRefSpeedup
  , getTrainSpeedup
  , getCompilationTime
  , getCodeSize
  , getRefEnergyUsage
  , getTrainEnergyUsage
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
getRefSpeedup :: CacheFile -> Maybe BS.ByteString
getRefSpeedup cf = getCacheFileContents "exec_times_ref.csv"


--------------------------------------------------------------------------------
-- | Get the compilation time data from the cached file
getCompilationTime :: CacheFile -> Maybe BS.ByteString
getCompilationTime cf = getCacheFileContents "comp_times.csv"
        

--------------------------------------------------------------------------------
-- | Get the code size data from the cached file 
getCodeSize :: CacheFile -> Maybe BS.ByteString
getCodeSize cf = getCacheFileContents "code_sizes.csv"


--------------------------------------------------------------------------------
-- | Get the energy usage data from the cached file
getRefEnergyUsage :: CacheFile -> Maybe BS.ByteString
getRefEnergyUsage cf = getCacheFileContents "energy_usage_ref.csv"


--------------------------------------------------------------------------------
-- | Get the energy usage data from the cached file
getTrainEnergyUsage :: CacheFile -> Maybe BS.ByteString
getTrainEnergyUsage cf = getCacheFileContents "energy_usage_train.csv"


--------------------------------------------------------------------------------
-- | Get the contents of a normal file given by its filename (as a suffix)
getCacheFileContents :: CacheFile -> FilePathSuffix -> Maybe BS.ByteString
getCacheFileContents cf fp = do
    e <- findEntry fp (cacheFileEntries cf)
    case TE.entryContent e of
        TE.NormalFile bs _ -> Just bs
        _                  -> Nothing


