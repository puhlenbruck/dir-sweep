module Cleanup.Options (Options(..), SubDirMode(..)) where

data Options = Options 
  { dirs :: [FilePath]
  , maxKeepCount :: Maybe Int
  , subDirMode :: SubDirMode
  , dryRun :: Bool
  , verbose :: Bool
  } deriving (Show)

data SubDirMode = File | Ignore | Recursive
  deriving (Read, Show)
