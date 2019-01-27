{-# LANGUAGE NamedFieldPuns #-}
module Sweep.Filter (filterFiles, FilterOptions, validatedFilterOptions) where

import           Control.Applicative            ( liftA2 )
import           Data.List                      ( sortOn )
import           Data.Ord                       ( Down(..) )
import           Data.Maybe                     ( Maybe
                                                , maybe
                                                , fromMaybe
                                                )
import           Data.Time.Clock                ( UTCTime )

import           Sweep.Files                    ( FileAndModTime(..) )

data FilterOptions = FilterOptions{thresholdTime :: Maybe UTCTime, maxKeep :: Maybe Int, minKeep :: Maybe Int}
  deriving (Show)

validatedFilterOptions :: Maybe UTCTime -> Maybe Int -> Maybe Int -> Either String FilterOptions
validatedFilterOptions thresholdTime maxKeep minKeep
  | maxLessThanMin = Left "MaxKeep cannot be less than MinKeep"
  | otherwise      = Right FilterOptions {thresholdTime , maxKeep , minKeep }
  where maxLessThanMin = fromMaybe False (liftA2 (<) maxKeep minKeep)

filterFiles :: FilterOptions -> [FileAndModTime] -> [FileAndModTime]
filterFiles FilterOptions { thresholdTime, maxKeep, minKeep } files = go minKeepValue $ drop minKeepValue $ sortOn
  (Down . modifyTime)
  files
 where
  minKeepValue = fromMaybe 0 minKeep
  go count (x : xs) | maybe False (count >=) maxKeep || maybe False (modifyTime x <) thresholdTime = reverse (x : xs)
                    | otherwise = go (count + 1) xs
  go count [] = []
