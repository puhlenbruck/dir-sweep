module Main where

import System.Directory
import Data.Time.Clock

main :: IO ()
main = do
  files <- listFiles "/home/peter/Downloads"
  mapM_ (print) files

data FileTime = FileTime {name :: FilePath, modifyTime :: UTCTime}
  deriving (Show)

thresholdTime :: DiffTime -> IO UTCTime
thresholdTime timePeriod = do
  currentTime <- getCurrentTime
  let nominalTimePeriod = realToFrac timePeriod
  return $ addUTCTime nominalTimePeriod currentTime


listFiles :: FilePath -> IO [FileTime]
listFiles dir = do
  files <- listDirectory dir
  let fullPaths = fmap (\name -> dir ++ "/" ++ name) files
  mapM (fileWithModTime) fullPaths

fileWithModTime :: FilePath -> IO FileTime
fileWithModTime file = do
  modTime <- getModificationTime file
  return $ FileTime file modTime
