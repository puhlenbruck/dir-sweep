module Sweep.Options (Options(..), SubDirMode(..)) where

import           Data.Time.Clock                ( DiffTime )

data Options = Options
  { dirs :: [FilePath]
  , thresholdAge :: Maybe DiffTime
  , maxKeepCount :: Maybe Int
  , minKeepCount :: Maybe Int
  , subDirMode :: SubDirMode
  , dryRun :: Bool
  , verbose :: Bool
  } deriving (Show)

data SubDirMode = File | Ignore | Recursive
  deriving (Read, Show)
