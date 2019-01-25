{-# LANGUAGE NamedFieldPuns #-}
module Sweep(module Sweep) where

import Control.Applicative ((<$>))
import Control.Monad (when, filterM, unless)
import System.Directory
import System.Exit (die)
import System.FilePath ((</>))

import Sweep.CLI as Sweep
import Sweep.Files as Sweep
import Sweep.Filter as Sweep
import Sweep.Messaging as Sweep
import Sweep.Options as Sweep
import Sweep.Time as Sweep

run :: IO ()
run = do
  opts@Options{dirs, maxKeepCount, minKeepCount, thresholdAge, verbose} <- getCommandLineOptions
  thresholdTime <- getThresholdTime thresholdAge
  filterOptions <- either die pure $ validatedFilterOptions thresholdTime maxKeepCount minKeepCount
  when verbose $ infoPrint opts
  mapM_ (runForDir opts filterOptions) dirs
  where
    getThresholdTime (Just x) = Just <$> calculateThresholdTime x
    getThresholdTime Nothing = return Nothing

runForDir :: Options -> FilterOptions -> FilePath -> IO ()
runForDir opts@Options{dryRun, verbose} filterOptions dir = do
  candidateFiles <- filesForDir opts dir
  let filesToDelete = [name file | file <- filterFiles filterOptions candidateFiles]
  when dryRun $ mapM_ putStrLn filesToDelete
  unless dryRun $ deleteFiles verbose filesToDelete
