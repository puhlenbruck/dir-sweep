{-# LANGUAGE NamedFieldPuns #-}
module Cleanup(module Cleanup) where

import Control.Applicative ((<$>))
import Control.Monad (when, filterM, unless)
import Data.Time.Clock
import Data.List (sortOn)
import Data.Ord
import Data.Maybe
import System.Directory
import System.FilePath ((</>))

import Cleanup.CLI as Cleanup
import Cleanup.Files as Cleanup
import Cleanup.Messaging as Cleanup
import Cleanup.Options as Cleanup
import Cleanup.Time as Cleanup
import Cleanup.Util as Cleanup

run :: IO ()
run = do
  opts@Options{dirs, thresholdAge, verbose} <- getCommandLineOptions
  thresholdTime <- getThresholdTime thresholdAge
  when verbose $ infoPrint opts
  mapM_ (runForDir opts) dirs
  where
    getThresholdTime (Just x) = Just <$> calculateThresholdTime x
    getThresholdTime Nothing = return Nothing

runForDir :: Options -> FilePath -> IO ()
runForDir opts@Options{dryRun} dir = do
  candidateFiles <- filesForDir opts dir
  let filesToDelete = [name file | file <- candidateFiles]
  when dryRun $ mapM_ print filesToDelete
  unless dryRun $ deleteFiles filesToDelete

filterFiles :: Maybe UTCTime -> Maybe Int -> Maybe Int -> [FileAndModTime] -> [FileAndModTime]
filterFiles thresholdTime maxKeep minKeep = go 0
  where go count (x:xs) | maybe False (count <) minKeep = x : go (count + 1) xs
                        | maybe False (count >) maxKeep = xs
                        | maybe False (modifyTime x <) thresholdTime = go count xs
                        | otherwise = x : go (count + 1) xs