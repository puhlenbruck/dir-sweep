{-# LANGUAGE NamedFieldPuns #-}
module Cleanup(module Cleanup) where

import Control.Applicative ((<$>))
import Control.Monad (when, filterM, unless)
import Data.Time.Clock
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Maybe (Maybe, maybe, fromMaybe)
import System.Directory
import System.FilePath ((</>))

import Cleanup.CLI as Cleanup
import Cleanup.Files as Cleanup
import Cleanup.Messaging as Cleanup
import Cleanup.Options as Cleanup
import Cleanup.Time as Cleanup

import Debug.Trace

run :: IO ()
run = do
  opts@Options{dirs, maxKeepCount, minKeepCount, thresholdAge, verbose} <- getCommandLineOptions
  thresholdTime <- getThresholdTime thresholdAge
  let filterOptions = FilterOptions {thresholdTime, maxKeep=maxKeepCount, minKeep=minKeepCount}
  when verbose $ infoPrint opts
  mapM_ (runForDir opts filterOptions) dirs
  where
    getThresholdTime (Just x) = Just <$> calculateThresholdTime x
    getThresholdTime Nothing = return Nothing

runForDir :: Options -> FilterOptions -> FilePath -> IO ()
runForDir opts@Options{dryRun} filterOptions dir = do
  candidateFiles <- filesForDir opts dir
  let filesToDelete = [name file | file <- filterFiles filterOptions candidateFiles]
  when dryRun $ mapM_ print filesToDelete
  unless dryRun $ deleteFiles filesToDelete

data FilterOptions = FilterOptions{thresholdTime :: Maybe UTCTime, maxKeep :: Maybe Int, minKeep :: Maybe Int}
  deriving (Show)

filterFiles :: FilterOptions -> [FileAndModTime] -> [FileAndModTime]
filterFiles FilterOptions{thresholdTime, maxKeep, minKeep} files = go minKeepValue  $ drop minKeepValue $ sortOn (Down . modifyTime) files
  where minKeepValue = fromMaybe 0 minKeep
        go count (x:xs) | maybe False (count >=) maxKeep || maybe False (modifyTime x <) thresholdTime = reverse (x:xs)
                        | otherwise                                                                    = go (count + 1) xs
        go count [] = []