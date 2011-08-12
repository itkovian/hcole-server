{-# LANGUAGE OverloadedStrings #-}
{-
 
Datatype to abstract away the data stored in the COLE files. We
need this to have a non-general function type in the MonadFSCache 
class. All accesses to the data should pass through here. I think.

-}

module Cole.ColeData
  ( ColeData
  , mkColeData
  ) where

import           Control.Monad (join, liftM)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC (lines, unlines)
import           Data.ByteString.Internal (isSpaceWord8)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (pack)
import qualified Text.CSV.ByteString as CSVB

import Cole.Files.Cache

-------------------------------------------------------------------
-- | Data type for the COLE information. FIXME: should also parse the 
-- ByteString and get real data out of it.
data ColeData = ColeData
  { coleRefSpeedup       :: Maybe CSVB.CSV
  , coleTrainSpeedup     :: Maybe CSVB.CSV
  , coleCompilationTime  :: Maybe CSVB.CSV
  , coleCodeSize         :: Maybe CSVB.CSV
  , coleRefEnergyUsage   :: Maybe CSVB.CSV
  , coleTrainEnergyUsage :: Maybe CSVB.CSV
  }


mkColeData :: FilePath -> IO ColeData
mkColeData fp = do 
  cacheFile <- mkCacheFile fp
  return $ ColeData { coleRefSpeedup       = join $ liftM convertToCSV $ getRefSpeedup cacheFile
                    , coleTrainSpeedup     = join $ liftM convertToCSV $ getTrainSpeedup cacheFile
                    , coleCompilationTime  = join $ liftM convertToCSV $ getCompilationTime cacheFile
                    , coleCodeSize         = join $ liftM convertToCSV $ getCodeSize cacheFile
                    , coleRefEnergyUsage   = join $ liftM convertToCSV $ getRefEnergyUsage cacheFile
                    , coleTrainEnergyUsage = join $ liftM convertToCSV $ getTrainEnergyUsage cacheFile
                    }
    where convertToCSV = CSVB.parseCSV                        -- Read into a CSV structure
                       . BSC.unlines                          -- Back to a single ByteString
                       . map (BS.filter (not . isSpaceWord8)) -- Fixoring @boegels stupid format :-)
                       . BSC.lines                            -- Split so we do not remove newlines ^  
                       . BS.concat                            -- String them together (they are smaller than 64KiB
                       . BSL.toChunks                         -- Make it strict.

instance A.ToJSON ColeData where
  toJSON cd = A.object [ "coleRefSpeedup"       A..= coleRefSpeedup cd
                       , "coleTrainSpeedup"     A..= coleTrainSpeedup cd
                       , "coleCompilationTime"  A..= coleCompilationTime cd
                       , "coleCodeSize"         A..= coleCodeSize cd
                       , "coleRefEnergyUsage"   A..= coleRefEnergyUsage cd
                       , "coleTrainEnergyUsage" A..= coleTrainEnergyUsage cd
                       ]

-- FIXME: Perhaps we should also have a FromJSON instance?


