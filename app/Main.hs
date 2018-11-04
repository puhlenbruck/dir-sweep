module Main where

import Control.Monad (when)
import Data.Time.Clock
import Data.List (sortOn)
import Data.Ord
import Data.Maybe
import Options.Applicative(execParser)
import System.Directory
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
  files <- sortOn (Down . modifyTime) <$> listFiles dir
  let filesToRemove = maybeDrop (maxKeepCount opts) files
  mapM_ print filesToRemove

maybeDrop :: Maybe Int -> [a] -> [a]
maybeDrop (Just n) list = drop n list
maybeDrop Nothing _ = []

data FileAndModTime = FileAndModTime {name :: FilePath, modifyTime :: UTCTime}
  deriving (Show)

thresholdTime :: DiffTime -> IO UTCTime
thresholdTime timePeriod = do
  currentTime <- getCurrentTime
  let nominalTimePeriod = realToFrac timePeriod
  return $ addUTCTime nominalTimePeriod currentTime

listFiles :: FilePath -> IO [FileAndModTime]
listFiles dir = do
  files <- listDirectory dir
  let fullPaths = fmap (\name -> dir ++ "/" ++ name) files
  mapM fileWithModTime fullPaths

fileWithModTime :: FilePath -> IO FileAndModTime
fileWithModTime file = do
  modTime <- getModificationTime file
  return $ FileAndModTime file modTime
