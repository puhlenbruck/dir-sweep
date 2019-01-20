module Cleanup.Time where

import Data.Time.Clock

thresholdTime :: DiffTime -> IO UTCTime
thresholdTime timePeriod = do
  currentTime <- getCurrentTime
  let nominalTimePeriod = realToFrac timePeriod
  return $ addUTCTime nominalTimePeriod currentTime