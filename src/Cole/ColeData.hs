{-
 
Datatype to abstract away the data stored in the COLE files. We
need this to have a non-general function type in the MonadFSCache 
class. All accesses to the data should pass through here. I think.

-}

module Cole.ColeData
  ( ColeData
  , mkColeData
  ) where

import qualified Data.ByteString.Lazy as BS

import Cole.Files.Cache

-------------------------------------------------------------------
-- | Data type for the COLE information. FIXME: should also parse the 
-- ByteString and get real data out of it.
data ColeData = ColeData
  { coleRefSpeedup       :: Maybe BS.ByteString
  , coleTrainSpeedup     :: Maybe BS.ByteString
  , coleCompilationTime  :: Maybe BS.ByteString
  , coleCodeSize         :: Maybe BS.ByteString
  , coleRefEnergyUsage   :: Maybe BS.ByteString
  , coleTrainEnergyUsage :: Maybe BS.ByteString
  }


mkColeData :: FilePath -> IO ColeData
mkColeData fp = do 
  cacheFile <- mkCacheFile fp
  return $ ColeData { coleRefSpeedup       = getRefSpeedup cacheFile
                    , coleTrainSpeedup     = getTrainSpeedup cacheFile
                    , coleCompilationTime  = getCompilationTime cacheFile
                    , coleCodeSize         = getCodeSize cacheFile
                    , coleRefEnergyUsage   = getRefEnergyUsage cacheFile
                    , coleTrainEnergyUsage = getTrainEnergyUsage cacheFile
                    }

  



