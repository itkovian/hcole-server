{-# LANGUAGE OverloadedStrings #-}
{-

Implementation of a watchdog for the COLE file system cache.

-}


module Cole.ColeWatchdog
  ( coleWatchdog
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import qualified Database.HDBC as HDBC
import           System.Directory
import           System.FilePath ((</>))

import qualified Cole.Cole as Cole
import qualified Cole.ColeDB as ColeDB

coleWatchdog :: HDBC.ConnWrapper -- ^ Connection to the database 
             -> FilePath    -- ^ Directory where cached files can be found
             -> IO ()       -- ^ Resulting type, we will launch this from a forkIO
coleWatchdog conn cacheDir = do
    results <- HDBC.quickQuery' conn "SELECT key FROM experiments WHERE state=\"ColeExperimentBusy\"" [] 
    forM_ results $ \[result] -> do let sequence = Cole.ColeSequence $ HDBC.fromSql result 
                                    exist <- doesFileExist $ cacheDir </> (Cole.cacheFilename sequence)
                                    when exist $ void $ ColeDB.updateSequenceToDone conn sequence
    threadDelay $ 5 * 60 * 1000 * 1000 
    coleWatchdog conn cacheDir
    

