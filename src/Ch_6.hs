{-# LANGUAGE TemplateHaskell #-}
module Ch_6(numOfDaysUntilFirstThaw,
           warmestTempInFirstFourDays)
  where

import Control.Lens


type TemperatureMeasurements = [Int]
numOfDaysUntilFirstThaw :: TemperatureMeasurements -> Int
numOfDaysUntilFirstThaw = lengthOf (takingWhile  (<= 0) folded)

warmestTempInFirstFourDays :: TemperatureMeasurements -> Maybe Int
warmestTempInFirstFourDays = error "x"
