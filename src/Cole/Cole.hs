{-# LANGUAGE OverloadedStrings #-}
{-

General stuff for COLE.

-}

module Cole.Cole 
  ( ColeSequence (..)
  , getConfigInfo
  , cacheFilename
  )
  where

--import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
--import qualified Data.Text.Encoding as TE
import           System.FilePath ((</>))
import           System.Environment (getEnv)
import           System.IO.Unsafe (unsafePerformIO)

-- Wrapper around the ByteString to represent a sequence
newtype ColeSequence = ColeSequence { runSequence :: T.Text }
newtype ColeConfig = ColeConfig { runColeConfig :: M.Map T.Text T.Text }

-- Configuration for the COLE service
-- This is not exported for now
coleConfig :: ColeConfig
coleConfig = ColeConfig $ M.fromList $ map (\(k, v) -> (T.pack k, T.pack v)) $
    [ ( "ColeExperimentHome",         (home </> "work/det_opt_flags_LLVM/experiments/train-WEKA-modelByBenchmark_by-COLE_gastly_LLVM-svn"))
    , ( "ColeExperimentCache",        (home </> "work/det_opt_flags_LLVM/experiments/train-WEKA-modelByBenchmark_by-COLE_gastly_LLVM-svn/cole-llvm_experiments_CACHE"))
    , ( "ColeExperimentSubmitScript", "submit_experiments_fast_coleserver.sh")
    ]
  where home = unsafePerformIO $ getEnv "HOME" -- FIXME: this is fugly. 


-- Getters for the configuration
getConfigInfo :: T.Text -> Maybe T.Text
getConfigInfo key = M.lookup key (runColeConfig coleConfig)


-- Make the filename of the cached file from the given sequence
cacheFilename :: ColeSequence -> FilePath
cacheFilename sequence = "key_" ++ T.unpack (runSequence sequence) ++ ".tgz" 

