module Cleanup.Files (FileAndModTime(..), deleteFiles, filesForDir) where

import Control.Monad (when, filterM, unless)
import Data.List (sortOn)
import Data.Maybe
import Data.Ord
import Data.Time.Clock (UTCTime)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory, removeDirectory, removeFile)
import System.FilePath ((</>))

import Cleanup.Messaging
import Cleanup.Options


data FileAndModTime = FileAndModTime {name :: FilePath, modifyTime :: UTCTime} 
  deriving (Show)

fileWithModTime :: FilePath -> IO FileAndModTime
fileWithModTime file = do
  modTime <- getModificationTime file
  return $ FileAndModTime file modTime

filesForDir :: Options -> FilePath -> IO [FileAndModTime]
filesForDir opts dir = do
  dirList <- listDirWithFullPaths dir
  files <- handleDirectories (subDirMode opts) dirList
  mapM fileWithModTime files 

listDirWithFullPaths :: FilePath -> IO [FilePath]
listDirWithFullPaths dir = do
    files <- listDirectory dir
    return $ map (dir </>) files

handleDirectories :: SubDirMode -> [FilePath] -> IO [FilePath]
handleDirectories File files = return files
handleDirectories Ignore files = filterM doesFileExist files
handleDirectories Recursive files = listDirectoriesRecursively files

listDirectoriesRecursively :: [FilePath] -> IO [FilePath]
listDirectoriesRecursively paths = fmap concat (mapM list paths)
  where list path = do
          isDirectory <- doesDirectoryExist path
          if isDirectory
            then listDirectoriesRecursively =<< listDirectory path
            else return [path]

deleteFiles :: [FilePath] -> IO ()
deleteFiles = mapM_ deleteFileOrDirectory

deleteFileOrDirectory :: FilePath -> IO ()
deleteFileOrDirectory path = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then deleteDirectory path
    else removeFile path
  where
    deleteDirectory path = do
      contents <- listDirWithFullPaths path
      case contents of
        [] -> removeDirectory path
        _  -> deleteFiles contents >> removeDirectory path