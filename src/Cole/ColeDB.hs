{-# LANGUAGE MultiParamTypeClasses #-}
{-

Database stuffs for the Cole DB to keep track of experiments

-}

module Cole.ColeDB
  ( ColeDBExperimentState (..)
  , Cole.ColeDB.lookup
  ) where

import           Data.ByteString.Char8 (unpack)
import           Data.Convertible.Base (Convertible, ConvertError (..), safeConvert)
import qualified Database.HDBC as HDBC

import Cole.Cole

-------------------------------------------------------------------
-- Data type representing a query to the database
data ColeDBQueryResult = ColeDBQueryResult ColeDBExperimentState 

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
  safeConvert ColeExperimentDone    = Right (HDBC.SqlString "ColeExperimentDone")
  safeConvert ColeExperimentBusy    = Right (HDBC.SqlString "ColeExperimentBusy")
  -- safeConvert ColeExperimentUnknown = Right (HDBC.SqlString "ColeExperimentUnknown") -- we should not store these in the DB
  safeConvert ColeExperimentError   = Right (HDBC.SqlString "ColeExperimentError")
  safeConvert _                     = Left (ConvertError { convSourceValue  = "ColeExperimentUnknown"
                                                         , convSourceType   = "ColeDBExperimentState"
                                                         , convDestType     = "SqlString String"
                                                         , convErrorMessage = "Unknown state, should not be stored in the DB"
                                                         })
  
-- FIXME: this seems to be fugly
instance Convertible HDBC.SqlValue ColeDBExperimentState where
  safeConvert (HDBC.SqlByteString s) =
    case unpack s of 
        -- "ColeExperimentUnknown" -> Right ColeExperimentUnknown
        "ColeExperimentBusy"    -> Right ColeExperimentBusy
        "ColeExperimentDone"    -> Right ColeExperimentDone
        "ColeExperimentError"   -> Right ColeExperimentError
        _                       -> Left (ConvertError { convSourceValue  = unpack s
                                                        , convSourceType   = "SqlString String"
                                                        , convDestType     = "ColeDBExperimentState"
                                                        , convErrorMessage = "Cannot convert string to destination type"
                                                        })
  -- safeConvert _ = Right ColeExperimentDone

-------------------------------------------------------------------
-- Lookup of a sequence in the DB. Returns one of the 
-- possible states.
lookup :: HDBC.ConnWrapper -> ColeSequence -> IO ColeDBExperimentState
lookup conn sequence = do
    resultSet <- HDBC.quickQuery' conn "SELECT state FROM experiments WHERE key = ?" [HDBC.toSql $ runSequence sequence]
    case resultSet of
      (r:_) -> return $ HDBC.fromSql $ head r
      _     -> return $ ColeExperimentUnknown



