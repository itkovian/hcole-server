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
import           System.Directory (getDirectoryContents)

import           Snap.Extension
import           Snap.Types

-------------------------------------------------------------------
-- | The MonadCache class. This is the minimalcomplete interface:
class MonadSnap m => MonadFSCache m where
    -- | Check the cache for the given piece of information
    fsCacheRequest :: FilePath -> (FilePath -> a) -> m (Maybe a)

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
    fsCacheRequest filename fileTransformer = do 
        fsCacheState <- asks getFSCacheState
        fss <- liftIO $ getDirectoryContents (fsCacheDir fsCacheState)
        if filename `elem` fss then return $ Just "FILE FOUND" else return Nothing

    -- Does nothing at this point
    fsCacheInsert _ = return ()




