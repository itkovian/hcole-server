{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import           Data.ByteString.Char8 (unpack, pack)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Extension.HDBC
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist

import           Application
import           Splice.Cole
import           Snap.Extension.FileSystemCache

import qualified Cole.Cole as Cole
import qualified Cole.ColeDB as ColeDB
------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]

------------------------------------------------------------------------------
-- | Shows the cache contents, i.e., the keys that have been measured
cache :: Application ()
cache = heistLocal (bindSplices cacheSplices) $ render "colecache"
  where 
    cacheSplices = 
        [ ("colecache-placeholder", coleCachePlaceHolder)
        ]


------------------------------------------------------------------------------
-- | Requests that a sequence of optimisations be evaluated
-- If the sequence is aqvailable, the relevant results will be returned
-- Otherwise, a 404 error message is returned
sequence :: Application ()
sequence = do
    s <- Cole.ColeSequence <$> decodedParam "sequence"
    -- Get the connection to the database
    conn <- connWrapper
    -- check if the sequence exists in the cache
    experimentStatus <- liftIO $ ColeDB.lookup conn s
    -- FIXME: This should follow a different pattern. Once we have the DB
    -- added to the application, we first check the DB for the key. This
    -- has three possible results: (i) the sequence has been measured, i.e., the 
    -- data is available in the filesystem cache, (ii) the sequence is being 
    -- measured, thus we need not launch a new measurement, and (iii) the sequence 
    -- is unknown to the system, so we need to launch a measurement and update the
    -- DB accordingly.
    -- FIXME: All lazy stuff should probably become strict as we need the results
    -- anyways.
    case experimentStatus of
        ColeDB.ColeExperimentDone -> do 
            v <- fsCacheRequest (concat $ ["key_", unpack $ Cole.runSequence s, ".tgz"]) 
            case v of
                Just coleData -> do let jsonResponse = TLE.decodeUtf8 . A.encode . A.toJSON $ coleData
                                    modifyResponse $ setResponseCode 200 
                                                   . setContentType (pack "application/json")
                                                   . setContentLength (fromIntegral $ TL.length jsonResponse) --FIXME
                                    writeText . TL.toStrict $ jsonResponse 
                Nothing -> do modifyResponse $ setResponseCode 404
                                             . setContentLength 3
        ColeDB.ColeExperimentBusy -> undefined
        ColeDB.ColeExperimentUnknown -> undefined


  where
    decodedParam p = fromMaybe "" <$> getParam p



------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (TE.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route [ ("/",                      index)
             , ("/cache",                 cache)
             , ("/sequence/:sequence",    Site.sequence)
             , ("/echo/:stuff",           echo)
             ]
       <|> serveDirectory "resources/static"
