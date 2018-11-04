module Main where

import Control.Monad (when, filterM)
import Data.Time.Clock
import Data.List (sortOn)
import Data.Ord
import Data.Maybe
import Options.Applicative(execParser)
import System.Directory
import System.FilePath ((</>))
import System.IO (hPrint, stderr)

import CLI

main :: IO ()
main = do
  opts <- execParser programOpts
  when (verbose opts) $ verboseOutput opts
  sequence_ $ fmap (runForDir opts) (dirs opts)

verboseOutput :: Show a => a -> IO ()
verboseOutput = hPrint stderr

runForDir :: Options -> FilePath -> IO ()
runForDir opts dir = do
  files <- sortOn (Down . modifyTime) <$> listFiles opts dir
  let filesToRemove = maybeDrop (maxKeepCount opts) files
  mapM_ print filesToRemove

maybeDrop :: Maybe Int -> [a] -> [a]
maybeDrop (Just n) = drop n
maybeDrop Nothing  = id

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
listAllFiles files = fmap concat $ sequence $ map subFiles files

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
