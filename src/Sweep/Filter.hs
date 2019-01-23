{-# LANGUAGE NamedFieldPuns #-}
module Sweep.Filter where

import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Time.Clock(UTCTime)

import Sweep.Files(FileAndModTime(..))

data FilterOptions = FilterOptions{thresholdTime :: Maybe UTCTime, maxKeep :: Maybe Int, minKeep :: Maybe Int}
  deriving (Show)

filterFiles :: FilterOptions -> [FileAndModTime] -> [FileAndModTime]
filterFiles FilterOptions{thresholdTime, maxKeep, minKeep} files = 
  go minKeepValue $ drop minKeepValue $ sortOn (Down . modifyTime) files
  where minKeepValue = fromMaybe 0 minKeep
        go count (x:xs) | maybe False (count >=) maxKeep || 
                          maybe True (modifyTime x <) thresholdTime = reverse (x:xs)
                        | otherwise                                 = go (count + 1) xs
        go count [] = []