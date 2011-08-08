{-|

This file contains the splices required by the 
cole server application that are cole-specific.

-}

module Splice.Cole 
  ( coleCachePlaceHolder
  , sequenceValue
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import           System.Directory (getDirectoryContents)
import           Text.Templating.Heist
import           Text.XmlHtml

import           Application

coleCachePlaceHolder :: Splice Application
coleCachePlaceHolder = do 
  dirListing <- liftIO . getDirectoryContents $ "/Users/ageorges/tmp/brol"
  return $ map (TextNode . T.pack) dirListing


sequenceValue :: Splice Application
sequenceValue = return $ [TextNode $ (T.pack "So you want to try sequence: ") ]

