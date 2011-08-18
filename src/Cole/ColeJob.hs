{-# LANGUAGE OverloadedStrings #-}
{-
 
Module containing the implementation to start up a new COLE job.

-}

module Cole.ColeJob
  ( launchJob
  ) where


import           Control.Concurrent
import qualified Data.ByteString as BS
import           Database.HDBC (ConnWrapper)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.Cmd
import           System.Directory
import           System.Exit
import           System.FilePath ((</>))
import           System.IO
import           System.Posix.Env

import           Cole.Cole
import           Cole.ColeDB


-------------------------------------------------------------------
-- Lauch a new job. This writes the desired sequence to a temporary
-- file and then executes the COLE job submitter. Upon success, it
-- also makes sure that the correct entry is stored in the database
-- detailing that we are currently busy with that sequence.
launchJob :: ConnWrapper   -- ^ Connection to the database to store the resulting information 
          -> ColeSequence  -- ^ The sequence that will need to be evaluated
          -> IO ThreadId   -- ^ Resulting thread id of the spawned job.
launchJob conn sequence = do
    -- create temporary file and store the sequence in it
    currentTempDir <- catch (getTemporaryDirectory) (\_ -> return ".")
    (jobTempFile, jobTempH) <- openTempFile currentTempDir "hcole-server.job"
    BS.hPut jobTempH $ TE.encodeUtf8 $ runSequence sequence
    hClose jobTempH
     
    -- fire up the job; the Cole module holds the location details?
    let jobScript = (T.unpack . fromJust $ getConfigInfo "ColeExperimentHome") </> (T.unpack . fromJust $ getConfigInfo "ColeExperimentSubmitScript") 
    forkIO $ do path <- getEnv "PATH"
                putEnv $ "PATH=$PATH:" ++ (T.unpack . fromJust $ getConfigInfo "ColeExperimentHome") 
                exitCode <- rawSystem  jobScript [jobTempFile]
                insertCode <- case exitCode of
                                ExitSuccess   -> insertLaunchedSequence conn sequence
                                ExitFailure e -> insertErrorSequence conn sequence "Failure launching the job" e
                return ()

