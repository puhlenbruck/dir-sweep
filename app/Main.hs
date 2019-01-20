module Main where

import Cleanup (run)
import CLI (getCommandLineOptions)

main :: IO ()
main = do
  opts <- getCommandLineOptions
  run opts
