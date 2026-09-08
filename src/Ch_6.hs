{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Ch_6(numOfDaysUntilFirstThaw,
           warmestTempInFirstFourDays,
           nextTempAfterWarmestTempInFirstFourDays,
           numOfConsecDaysOfBelowFreezingWeather,
           tempsFromFirstThawToNextFreeze,
           tempsFromFirstThawToFinalFreeze,
           Card(..),
           Aura(..),
           Move(..),
           namesThatStartWithS)
  where

import Control.Lens
import Data.List.Lens(prefixed)

type TemperatureMeasurements = [Int]
numOfDaysUntilFirstThaw :: TemperatureMeasurements -> Int
numOfDaysUntilFirstThaw = lengthOf (takingWhile  (<= 0) folded)

warmestTempInFirstFourDays :: TemperatureMeasurements -> Maybe Int
warmestTempInFirstFourDays = maximumOf (taking 4 folded)

nextTempAfterWarmestTempInFirstFourDays :: TemperatureMeasurements -> Maybe Int
nextTempAfterWarmestTempInFirstFourDays = (^? dropping 1 warmerTemps)
  where
    warmerTemps = droppingWhile (/= 4) folded

-- Calculates the number of consecutive days of below freezing weather
-- there were starting from the end of the sample
numOfConsecDaysOfBelowFreezingWeather :: TemperatureMeasurements -> Int
numOfConsecDaysOfBelowFreezingWeather = lengthOf daysWithFreezingWeather
  where
    daysWithFreezingWeather = takingWhile (< 0) (backwards folded)

tempsFromFirstThawToNextFreeze :: TemperatureMeasurements -> TemperatureMeasurements
tempsFromFirstThawToNextFreeze = (^.. takingWhile (> 0) tempsAfterFirstFreeze)
  where
    tempsAfterFirstFreeze = droppingWhile (< 0) folded


trimmingWhile :: (a -> Bool) -> Fold s a -> Fold s a
trimmingWhile predicate = backwards . droppingWhile predicate  . backwards  . droppingWhile predicate

tempsFromFirstThawToFinalFreeze :: TemperatureMeasurements -> TemperatureMeasurements
tempsFromFirstThawToFinalFreeze = (^.. trimmingWhile isFreezing folded)
  where
    isFreezing = (< 0)


data Card = Card { _name :: String ,
                   _aura :: Aura ,
                   _holo :: Bool ,
                   _moves :: [Move]} deriving (Show, Eq)

data Aura
  = Wet
  | Hot
  | Spark
  | Leafy deriving (Show, Eq)

data Move = Move { _moveName :: String , _movePower :: Int} deriving (Show, Eq)

makeLenses ''Card
makeLenses ''Move

namesThatStartWithS :: [Card] -> [String]
namesThatStartWithS = (^.. folded . filteredBy (name . prefixed "S") . name)
