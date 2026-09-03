{-# LANGUAGE TemplateHaskell #-}
module Ch_3 (someFunc,
             User(..),
             fullName)
where

import Control.Lens

data User = User { _firstName :: String
                 , _lastName :: String
                 , _email :: String}
          deriving (Show)
makeLenses ''User

fullName = error "x"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
