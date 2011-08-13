{-# LANGUAGE MultiParamTypeClasses #-}
{-

Database stuffs for the Cole DB to keep track of experiments

-}

module Cole.ColeDB
  ( ColeDBExperimentState

  ) where

import           Data.Convertible.Base (Convertible, ConvertError (..), safeConvert)
import qualified Database.HDBC as HDBC

-------------------------------------------------------------------
-- Data type representing the state an experiment can be in.
data ColeDBExperimentState = ColeExperimentUnknown -- ^ This experiment has not been started yet.
                           | ColeExperimentBusy    -- ^ This experiment is currently running
                           | ColeExperimentDone    -- ^ This experiment has been completed, results should be in the file cache
                           -- FIXME: we may want to have an error message? Or an actual Error?
                           | ColeExperimentError   -- ^ This experiment resulted in an error
                            deriving (Show, Read)

-------------------------------------------------------------------
-- We need to be able to store these values in the database and 
-- get them back out, so should be an instance of HDBC.Convertible
instance Convertible ColeDBExperimentState HDBC.SqlValue where
  safeConvert ColeExperimentUnknown = Right (HDBC.SqlString "ColeExperimentUnknown")
  safeConvert ColeExperimentBusy    = Right (HDBC.SqlString "ColeExperimentBusy")
  safeConvert ColeExperimentDone    = Right (HDBC.SqlString "ColeExperimentDone")
  safeConvert ColeExperimentError   = Right (HDBC.SqlString "ColeExperimentError")
  safeConvert _                     = Left (ConvertError { convSourceValue  = "unknown"
                                                         , convSourceType   = "ColeDBExperimentState"
                                                         , convDestType     = "SqlString String"
                                                         , convErrorMessage = "Unknown state"
                                                         })
  
-- FIXME: this seems to be fugly
instance Convertible HDBC.SqlValue ColeDBExperimentState where
  safeConvert (HDBC.SqlString s) 
    | s == "ColeExperimentUnknown" = Right ColeExperimentUnknown
    | s == "ColeExperimentBusy"    = Right ColeExperimentBusy
    | s == "ColeExperimentDone"    = Right ColeExperimentDone
    | s == "ColeExperimentError"   = Right ColeExperimentError
    | otherwise                    = Left (ConvertError { convSourceValue  = s
                                                        , convSourceType   = "SqlString String"
                                                        , convDestType     = "ColeDBExperimentState"
                                                        , convErrorMessage = "Cannot convert string to destination type"
                                                        })


