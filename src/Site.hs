{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Data.ByteString.Char8 (unpack, pack)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.JSON as JSON

import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist

import           Application
import           Splice.Cole
import           Snap.Extension.FileSystemCache

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
    s <- decodedParam "sequence"
    -- check if the sequence exists in the cache
    v <- fsCacheRequest (unpack s) (Just . id)
    case v of
      Just f -> do let jsonResponse = T.pack $ JSON.encode ("dit is een test" :: String, 1.0 :: Double)
                   modifyResponse $ setResponseCode 200 
                                  . setContentType (pack "application/json")
                                  . setContentLength (fromIntegral $ T.length jsonResponse) --FIXME
                   writeText $ jsonResponse 
      Nothing -> do modifyResponse $ setResponseCode 404
                                   . setContentLength 3


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
