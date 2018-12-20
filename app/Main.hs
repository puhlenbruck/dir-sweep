{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Monad (when, filterM, unless)
import Data.Time.Clock
import Data.List (sortOn)
import Data.Ord
import Data.Maybe
import Options.Applicative(execParser)
import System.Directory
import System.FilePath ((</>))

import CLI
import Util
import Messaging

main :: IO ()
main = do
  opts@Options{verbose, dryRun} <- execParser programOpts
  when verbose $ verbosePrint opts
  filesToDelete <- concat <$> traverse (filesForDir opts) (dirs opts)
  when dryRun $ mapM_ print filesToDelete
  unless dryRun $ deleteFiles filesToDelete

{- TODO: This is very simple and not robust.  Error handling and verbose output support will still be needed -}
deleteFiles :: [FileAndModTime] -> IO ()
deleteFiles files = mapM_ removeFile [name file | file <- files]

filesForDir :: Options -> FilePath -> IO [FileAndModTime]
filesForDir opts dir = do
  files <- sortOn (Down . modifyTime) <$> listFiles opts dir
  return $ maybeDrop (maxKeepCount opts) files

data FileAndModTime = FileAndModTime {name :: FilePath, modifyTime :: UTCTime}
  deriving (Show)

thresholdTime :: DiffTime -> IO UTCTime
thresholdTime timePeriod = do
  currentTime <- getCurrentTime
  let nominalTimePeriod = realToFrac timePeriod
  return $ addUTCTime nominalTimePeriod currentTime

listFiles :: Options -> FilePath -> IO [FileAndModTime]
listFiles opts dir = do
  dirList <- listDirWithFullPaths dir
  files <- handleDirectories (subDirMode opts) dirList
  mapM fileWithModTime files

handleDirectories :: SubDirMode -> [FilePath] -> IO [FilePath]
handleDirectories File files = return files
handleDirectories Ignore files = filterM doesFileExist files
handleDirectories Recursive files = listAllFiles files

listAllFiles :: [FilePath] -> IO [FilePath]
listAllFiles files = concat <$> traverse subFiles files

subFiles :: FilePath -> IO [FilePath]
subFiles file = do
  isDir <- doesDirectoryExist file
  if isDir
    then listAllFiles =<< listDirWithFullPaths file
    else return [file]

listDirWithFullPaths :: FilePath -> IO [FilePath]
listDirWithFullPaths dir = do
  files <- listDirectory dir
  return $ map (dir </>) files

fileWithModTime :: FilePath -> IO FileAndModTime
fileWithModTime file = do
  modTime <- getModificationTime file
  return $ FileAndModTime file modTime
