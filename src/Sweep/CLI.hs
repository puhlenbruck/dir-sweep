module Sweep.CLI (getCommandLineOptions) where

import Control.Arrow (left)
import Data.Char (toLower, toUpper)
import Data.Time.Clock (DiffTime)
import Data.Version (showVersion)
import Data.Semigroup ((<>))
import Options.Applicative
import Text.Read (readEither)

import Sweep.Options
import Sweep.Time(parseDuration)

import Paths_dir_sweep(version)

getCommandLineOptions :: IO Options
getCommandLineOptions = execParser programOpts

versionString :: String
versionString = showVersion version

programOpts :: ParserInfo Options
programOpts = info ( options <**> helper <**> versionOption)
  (fullDesc
  <> progDesc "Clean old files from a directory"
  <> header ("dir-sweep " ++ versionString ++ " - a temp directory cleaning tool"))

versionOption :: Parser (a -> a)
versionOption = infoOption versionString (short 'V' <> long "version" <> help "Show program version and exit")

options :: Parser Options
options = Options
  <$> some ( argument str (metavar "TARGET..." <> help "Directories to clean.  Results may be unexpected if given multiple directories in the same heirarchy" ))
  <*> option timeThreshold (short 't' <> long "time" <> help "Remove all files older than this threshold.  Uses the pattern <number><unit> eg `3d` for 3 days.  Available units are [s (seconds), mi (minutes), h (hours), d (days), w (weeks)]" <> value Nothing <> metavar "THRESHOLD")
  <*> option optionalInt (long "max-keep" <> help "The maximum number of files to keep.  At most this many files will not be deleted, even if they are younger than the threshold time." <> value Nothing <> metavar "MAX-KEEP")
  <*> option optionalInt (long "min-keep" <> help "The minimum number of files to keep.  At least this many files will not be deleted, even if they are older than the threshold time." <> value Nothing <> metavar "MIN-KEEP")
  <*> option dirMode (long "sub-dir" <> value File <> help subDirModeHelpMessage <> metavar "MODE")
  <*> switch (short 'd' <> long "dry-run" <> help "Do not delete files but print files that would be deleted.")
  <*> switch (short 'v' <> long "verbose" <> help "Enable verbose output.")
 
timeThreshold :: ReadM (Maybe DiffTime)
timeThreshold = eitherReader (fmap Just . parseDuration)

dirMode :: ReadM SubDirMode
dirMode = eitherReader parse
  where parse (first:rest) = readEither $ toUpper first : map toLower rest

optionalInt :: ReadM (Maybe Int)
optionalInt = eitherReader $ \s -> 
  left (\_ -> "Cannot parse max-keep value '" ++ s ++ "'. Must be an integer.") (Just <$> readEither s)

subDirModeHelpMessage :: String
subDirModeHelpMessage = "How to treat subdirectories. FILE (default), IGNORE, or RECURSIVE.  FILE will treat entire directory as a single file without considering it's contents."