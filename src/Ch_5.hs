{-# LANGUAGE TemplateHaskell #-}
module Ch_5 (Gate(..),
            Army(..),
            Kingdom(..),
            lens1
            )
where

import Control.Lens
import Control.Arrow((>>>))

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
