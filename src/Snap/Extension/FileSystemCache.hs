{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-

Implementation of the Cache typeclass by using the filesystem directly.

-}

module Snap.Extension.FileSystemCache (
    MonadFSCache (..)
  , FSCacheState
  , fsCacheLocation
  , HasFileSystemCacheState (..)
  , fsCacheInitializer
  ) where

import           Control.Monad.Reader
import qualified Data.ByteString.Lazy as BS
import           System.Directory (getDirectoryContents)
import           System.FilePath (combine)

import           Snap.Extension
import           Snap.Types

import           Cole.ColeData

-------------------------------------------------------------------
-- | The MonadFSCache class. This is the minimal complete interface:
class MonadSnap m => MonadFSCache m where
    -- | Check the cache for the given piece of information
    fsCacheRequest :: FilePath -> m (Maybe ColeData)

    -- | Insert the new piece of information in the cache
    fsCacheInsert :: FilePath -> m ()


data FSCacheState = FSCacheState 
  { fsCacheDir :: FilePath
  }

fsCacheLocation :: FSCacheState -> FilePath
fsCacheLocation = fsCacheDir


-------------------------------------------------------------------
-- | Class to indicate the application has a FileSystemState
class HasFileSystemCacheState s where
    getFSCacheState :: s -> FSCacheState
    setFSCacheState :: FSCacheState -> s -> s


-------------------------------------------------------------------
-- | Initializer for the file system cache
fsCacheInitializer :: FilePath -> Initializer FSCacheState
fsCacheInitializer fp = return $ FSCacheState { fsCacheDir = fp }


-------------------------------------------------------------------
-- FSCacheState is an instance of the InitializerState class
instance InitializerState FSCacheState where
    extensionId = undefined -- const $ "FSCache/FSCache"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()


-------------------------------------------------------------------
-- |
instance HasFileSystemCacheState s => MonadFSCache (SnapExtend s) where
    fsCacheRequest filename = do 
        fsCacheState <- asks getFSCacheState
        -- FIXME: this should be made cleaner.
        fss <- liftIO $ getDirectoryContents (fsCacheDir fsCacheState)
        if filename `elem` fss 
            then do cd <- liftIO $ mkColeData (combine (fsCacheDir fsCacheState) filename)
                    return $ Just cd
            else return Nothing

    -- Does nothing at this point
    fsCacheInsert _ = return ()




