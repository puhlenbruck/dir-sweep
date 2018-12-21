module CLI (getCommandLineOptions) where

import Control.Arrow (left)
import Data.Char (toLower, toUpper)
import Data.Version (showVersion)
import Data.Semigroup ((<>))
import Options.Applicative
import Text.Read (readEither)

import Cleanup
import Paths_cleanup (version)

getCommandLineOptions :: IO Options
getCommandLineOptions = execParser programOpts

programOpts :: ParserInfo Options
programOpts = info ( options <**> helper <**> versionOption)
  (fullDesc
  <> progDesc "Clean old files from a directory"
  <> header "cleanup - a temp directory cleaning tool")

versionOption :: Parser (a -> a)
versionOption = infoOption versionString (long "version" <> help "Show program version and exit")
  where versionString = showVersion version

options :: Parser Options
options = Options
  <$> some ( argument str (metavar "TARGET..." <> help "directories to clean." ))
  <*> option optionalInt (long "max-keep" <> help "the maximum number of files to keep." <> value Nothing <> metavar "MAX-KEEP")
  <*> option dirMode (long "sub-dir" <> value File <> help subDirModeHelpMessage <> metavar "MODE")
  <*> switch (short 'd' <> long "dry-run" <> help "Do not delete files but print files that would be deleted")
  <*> switch (short 'v' <> long "verbose" <> help "enable verbose output")
 

dirMode :: ReadM SubDirMode
dirMode = eitherReader parse
  where parse (first:rest) = readEither $ toUpper first : map toLower rest

optionalInt :: ReadM (Maybe Int)
optionalInt = eitherReader $ \s -> 
  left (\_ -> "Cannot parse max-keep value '" ++ s ++ "'. Must be an integer.") (Just <$> readEither s)

subDirModeHelpMessage :: String
subDirModeHelpMessage = "How to treat subdirectories. FILE (default), IGNORE, or RECURSIVE.  FILE will treat entire directory as a single file without considering it's contents."