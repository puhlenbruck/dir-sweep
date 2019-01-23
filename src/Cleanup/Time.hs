module Cleanup.Time(calculateThresholdTime, parseDuration) where
  
import Data.Char (isDigit, isSpace, toLower)
import Data.Either
import Data.Time.Clock
import Text.Read (readEither)
  
calculateThresholdTime :: DiffTime -> IO UTCTime
calculateThresholdTime timePeriod = do
  currentTime <- getCurrentTime
  let nominalTimePeriod = negate . abs . realToFrac $ timePeriod
  return $ addUTCTime nominalTimePeriod currentTime

parseDuration :: String -> Either String DiffTime
parseDuration str = do
  (time, unit) <- extractTimeAndUnit str
  Right $ secondsToDiffTime $ time * timeUnitSeconds unit

data TimeUnit = Second | Minute | Hour | Day | Week

parseUnit :: String -> Either String TimeUnit
parseUnit s = parse $ map toLower s
  where 
    parse "s" = Right Second
    parse "mi" = Right Minute
    parse "h" = Right Hour
    parse "d" = Right Day
    parse "w" = Right Week
    parse other = Left $ "Cannot parse '" ++ other ++ "' as time unit, available options are [s, mi, h, d, w]"

timeUnitSeconds :: Integral a => TimeUnit -> a
timeUnitSeconds Second = 1
timeUnitSeconds Minute = 60
timeUnitSeconds Hour = 3600
timeUnitSeconds Day = 86400
timeUnitSeconds week = 604800

extractTimeAndUnit :: (Read a, Integral a) => String -> Either String (a, TimeUnit)
extractTimeAndUnit str = do
  let (timeStr, unitStr) = extractParts str
  time <- readEither timeStr
  unit <- parseUnit unitStr
  return (time, unit)


extractParts :: String -> (String, String)
extractParts str = go [] (dropWhile isSpace str)
  where go acc (x:xs) | isDigit x = go (x : acc) xs
                      | otherwise = (reverse acc, x:xs)
