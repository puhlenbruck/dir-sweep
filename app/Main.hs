module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.Directory
import Data.Time.Clock
import Data.List (sortOn)
import Data.Ord
import Text.Read
import Data.Maybe

main :: IO ()
main = do
  opts <- execParser programOpts
  sequence_ $ fmap (runForDir opts) (dirs opts)

data Options = Options 
  { dirs :: [FilePath]
  , maxKeepCount :: Maybe Int
  }

programOpts = info (options <**> helper)
  (fullDesc
  <> progDesc "Clean old files from a directory"
  <> header "cleanup - a temp directory cleaning tool")

options :: Parser Options
options = Options
  <$> some ( argument str (metavar "TARGET..." <> help "directories to clean" ))
  <*> option optionalInt (long "max-keep" <> short 'n' <> help "the maximum number of files to keep" <> value Nothing <> metavar "MAX-KEEP")

optionalInt :: ReadM (Maybe Int)
optionalInt = eitherReader $ \s -> maybe (Left $ "Cannot parse max-keep value '" ++ s ++ "'. Must be an integer.") (Right . Just) (readMaybe s)

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
