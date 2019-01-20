{-# LANGUAGE NamedFieldPuns #-}
module Cleanup(module Cleanup) where

import Control.Monad (when, filterM, unless)
import Data.Time.Clock
import Data.List (sortOn)
import Data.Ord
import Data.Maybe
import System.Directory
import System.FilePath ((</>))

import Cleanup.Files as Cleanup
import Cleanup.Messaging as Cleanup
import Cleanup.Options as Cleanup
import Cleanup.Util as Cleanup

run :: Options -> IO()
run opts@Options{verbose, dryRun} = do
    when verbose $ infoPrint opts
    candidateFiles <- concat <$> traverse (filesForDir opts) (dirs opts)
    let filesToDelete = [name file | file <- candidateFiles]
    when dryRun $ mapM_ print filesToDelete
    unless dryRun $ deleteFiles filesToDelete