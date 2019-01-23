module Sweep.Files (FileAndModTime(..), deleteFiles, filesForDir) where

import Control.Monad (when, filterM, unless)
import Data.List (sortOn)
import Data.Maybe
import Data.Ord
import Data.Time.Clock (UTCTime)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory, removeDirectory, removeFile)
import System.FilePath ((</>))

import Sweep.Messaging
import Sweep.Options

data FileAndModTime = FileAndModTime {name :: FilePath, modifyTime :: UTCTime} 
  deriving (Show)

fileWithModTime :: FilePath -> IO FileAndModTime
fileWithModTime file = do
  modTime <- getModificationTime file
  return $ FileAndModTime file modTime

filesForDir :: Options -> FilePath -> IO [FileAndModTime]
filesForDir opts dir = do
  dirList <- listDirectoryWithFullPaths dir
  files <- handleDirectories (subDirMode opts) dirList
  mapM fileWithModTime files 

listDirectoryWithFullPaths :: FilePath -> IO [FilePath]
listDirectoryWithFullPaths dir = do
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
            then listDirectoriesRecursively =<< listDirectoryWithFullPaths path
            else return [path]

deleteFiles :: Bool -> [FilePath] -> IO ()
deleteFiles verbose = mapM_ (deleteFileOrDirectory verbose)

deleteFileOrDirectory :: Bool -> FilePath -> IO ()
deleteFileOrDirectory verbose path = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then deleteDirectory
    else deleteFile
  where
    deleteDirectory = do
      contents <- listDirectoryWithFullPaths path
      case contents of
        [] -> deleteEmptyDirectory
        _  -> deleteFiles verbose contents >> deleteEmptyDirectory
    deleteEmptyDirectory = do
      removeDirectory path
      when verbose $ infoMessage ("Deleted directory '" ++ path ++ "'")
    deleteFile = do 
      removeFile path
      when verbose $ infoMessage ("Deleted file '" ++ path ++ "'")

