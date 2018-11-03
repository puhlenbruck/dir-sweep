module CLI where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Read



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
  <*> option optionalInt (long "max-keep" <> help "the maximum number of files to keep" <> value Nothing <> metavar "MAX-KEEP")

optionalInt :: ReadM (Maybe Int)
optionalInt = eitherReader $ \s -> maybe (Left $ "Cannot parse max-keep value '" ++ s ++ "'. Must be an integer.") (Right . Just) (readMaybe s)
