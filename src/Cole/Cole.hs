{-

General stuff for COLE.

-}

module Cole.Cole 
  ( ColeSequence (..)
  )
  where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import           System.FilePath ((</>))
import           System.Environment (getEnv)
import           System.IO.Unsafe (unsafePerformIO)

-- Wrapper around the ByteString to represent a sequence
newtype ColeSequence = ColeSequence { runSequence :: BS.ByteString }
newtype ColeConfig = ColeConfig { runConfig :: M.Map BS.ByteString BS.ByteString }

-- Configuration for the COLE service
coleConfig :: ColeConfig
coleConfig = ColeConfig $ M.fromList $ map (\(k, v) -> (BS.pack k, BS.pack v)) $
    [ ( "ColeExperimentHome", (home </> "work/det_opt_flags_LLVM/train-WEKA-modelByBenchmark_by-COLE_gastly_LLVM-svn"))
    , ( "ColeExperimentSubmitScript", "submit_experiments_fast.sh")
    ]
  where home = unsafePerformIO $ getEnv "HOME" -- FIXME: this is fugly. 


