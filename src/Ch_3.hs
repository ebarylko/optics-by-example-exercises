{-# LANGUAGE TemplateHaskell #-}
module Ch_3 (someFunc,
             User(..),
             fullName)
where

import Data.List(intercalate)
import Control.Lens

data User = User { _firstName :: String
                 , _lastName :: String
                 , _email :: String}
          deriving (Show)
makeLenses ''User

fullName :: Lens' User String
--fullName = view firstName user (++) " " ++ view lastName user
fullName = lens getFullName (error "x") where
  getFullName = (intercalate " ")  <$>  (toList <$> view firstName  <*> view lastName)
  toList x y = [x, y]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
