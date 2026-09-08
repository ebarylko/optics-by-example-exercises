{-# LANGUAGE TemplateHaskell #-}
module Ch_6(numOfDaysUntilFirstThaw,
           warmestTempInFirstFourDays,
           nextTempAfterWarmestTempInFirstFourDays)
  where

import Control.Lens

type TemperatureMeasurements = [Int]
numOfDaysUntilFirstThaw :: TemperatureMeasurements -> Int
numOfDaysUntilFirstThaw = lengthOf (takingWhile  (<= 0) folded)

warmestTempInFirstFourDays :: TemperatureMeasurements -> Maybe Int
warmestTempInFirstFourDays = maximumOf (taking 4 folded)

nextTempAfterWarmestTempInFirstFourDays :: TemperatureMeasurements -> Maybe Int
nextTempAfterWarmestTempInFirstFourDays = (^? dropping 1 warmerTemps)
  where
    warmerTemps = droppingWhile (/= 4) folded
