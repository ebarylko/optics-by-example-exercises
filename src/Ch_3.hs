{-# LANGUAGE TemplateHaskell #-}
module Ch_3 (User(..),
             fullName,
             ProducePrices(..),
             lemonPrice,
             limePrice)
where

import Data.List(intercalate)
import Control.Lens
import Data.Char(isSpace)
import Control.Arrow((>>>))

data User = User { _firstName :: String
                 , _lastName :: String
                 , _email :: String}
          deriving (Show, Eq)
makeLenses ''User

fullName :: Lens' User String
fullName = lens getFullName setFullName where
  getFullName = (intercalate " ")  <$>  (toList <$> view firstName  <*> view lastName)
  setFullName user newFullName = set firstName (extractFirstName newFullName) user & set lastName (extractLastName newFullName)
  extractFirstName = takeWhile (not . isSpace)
  extractLastName = dropWhile (not . isSpace)  >>> drop 1
  toList x y = [x, y]


data ProducePrices = ProducePrices { _limePrice :: Float
                                   , _lemonPrice :: Float} deriving (Show, Eq)

limePrice :: Lens' ProducePrices Float
limePrice = lens getLimePrice setLimePrice
  where
    getLimePrice = _limePrice
    setLimePrice currPrices newPrice = currPrices {_limePrice =  max 0 newPrice}

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getLemonPrice setLemonPrice
  where
    getLemonPrice = _lemonPrice
    setLemonPrice currPrices newPrice = currPrices {_lemonPrice =  max 0 newPrice}

