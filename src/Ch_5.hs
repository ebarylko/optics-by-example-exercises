{-# LANGUAGE TemplateHaskell #-}
module Ch_5 (Gate(..),
            Army(..),
            Kingdom(..),
            lens1,
            lens2,
            lens3,
            lens4)
where

import Control.Lens
import Control.Arrow((>>>))
import Data.Char(toUpper)


data Gate = Gate { _open :: Bool
                 , _oilTemp :: Float}
          deriving (Show, Eq)
makeLenses ''Gate

data Army = Army { _archers :: Int ,
                   _knights :: Int} deriving (Show, Eq)
makeLenses ''Army

data Kingdom = Kingdom { _name :: String,
                         _army :: Army ,
                         _gate :: Gate} deriving (Show, Eq)
makeLenses ''Kingdom

lens1 :: Kingdom -> Kingdom
lens1 = name %~ (++ ": a perfect place") >>> army . knights .~ 42 >>> gate . open .~ False


lens2 :: Kingdom -> Kingdom
lens2 = name %~ (++ "instein") >>> army . knights .~ 26 >>> gate . oilTemp .~ 100 >>> army . archers .~ 17

lens3 :: Kingdom -> (String, Kingdom)
lens3 = name %~ (++ ": Home") >>> gate . oilTemp .~ 5 >>> name <<%~ (++ " of the talking Donkeys")

lens4 :: ((Bool, String), Float) -> ((Bool, String), Float)
lens4 = error "x"
