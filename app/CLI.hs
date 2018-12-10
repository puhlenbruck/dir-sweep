module CLI (programOpts, Options(..), SubDirMode(..)) where

import Control.Arrow (left)
import Data.Char (toLower, toUpper)
import Data.Semigroup ((<>))
import Options.Applicative
import Text.Read (readEither)


data Options = Options 
  { dirs :: [FilePath]
  , maxKeepCount :: Maybe Int
  , subDirMode :: SubDirMode
  , dryRun :: Bool
  , verbose :: Bool
  } deriving (Show)

data SubDirMode = File | Ignore | Recursive
  deriving (Read, Show)

dirMode :: ReadM SubDirMode
dirMode = eitherReader parse
  where parse (first:rest) = readEither $ (toUpper first) : (map toLower rest)


programOpts = info (options <**> helper)
  (fullDesc
  <> progDesc "Clean old files from a directory"
  <> header "cleanup - a temp directory cleaning tool")

options :: Parser Options
options = Options
  <$> some ( argument str (metavar "TARGET..." <> help "directories to clean." ))
  <*> option optionalInt (long "max-keep" <> help "the maximum number of files to keep." <> value Nothing <> metavar "MAX-KEEP")
  <*> option dirMode (long "sub-dir" <> value File <> help subDirModeHelpMessage <> metavar "MODE")
  <*> switch (short 'd' <> long "dry-run" <> help "Do not delete files but print files that would be deleted")
  <*> switch (short 'v' <> long "verbose" <> help "enable verbose output")

optionalInt :: ReadM (Maybe Int)
optionalInt = eitherReader $ \s -> 
  left (\_ -> "Cannot parse max-keep value '" ++ s ++ "'. Must be an integer.") (fmap Just $ readEither s)

subDirModeHelpMessage :: String
subDirModeHelpMessage = "How to treat subdirectories. FILE (default), IGNORE, or RECURSIVE.  FILE will treat entire directory as a single file without considering it's contents."