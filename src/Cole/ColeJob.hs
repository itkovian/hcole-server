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
    BS.writeFile jobTempFile $ TE.encodeUtf8 $ runSequence sequence
    hClose jobTempH
     
    -- fire up the job; the Cole module holds the location details?
    let jobScript = (T.unpack . fromJust $ getConfigInfo "ColeExperimentHome") </> (T.unpack . fromJust $ getConfigInfo "ColeExperimentSubmitScript") 
    forkIO $ do exitCode <- rawSystem jobScript [jobScript, jobTempFile]
                case exitCode of
                    ExitSuccess   -> do insertLaunchedSequence conn sequence
                                        return ()
                    ExitFailure e -> do insertErrorSequence conn sequence "Failure launching the job" e
                                        return ()

