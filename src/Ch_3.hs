{-# LANGUAGE TemplateHaskell #-}
module Ch_3 (User(..),
             fullName,
             ProducePrices(..),
             lemonPrice,
             limePrice,
             lemonPrice',
             limePrice')
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

toNonNegVal :: Float -> Float
toNonNegVal = max 0 

limePrice :: Lens' ProducePrices Float
limePrice = lens getLimePrice setLimePrice
  where
    getLimePrice = _limePrice
    setLimePrice currPrices newPrice = currPrices {_limePrice =  toNonNegVal newPrice}

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getLemonPrice setLemonPrice
  where
    getLemonPrice = _lemonPrice
    setLemonPrice currPrices newPrice = currPrices {_lemonPrice =  toNonNegVal newPrice}



-- Gets the current prices of lemons/limes and
-- adjusts the prices of both such that they are always nonnegative and
-- never more than 50 cents apart
lemonPrice' :: Lens' ProducePrices Float
lemonPrice' = lens (view lemonPrice) adjustLemonPrice
  where
    adjustLemonPrice currPrices newPrice = set lemonPrice newPrice currPrices & set limePrice (calcNewLimePrice (toNonNegVal newPrice) (view limePrice currPrices))
    calcNewLimePrice newLemonPrice currLimePrice
      | isFurtherThan50CentsApartFrom newLemonPrice currLimePrice && canMakeLimesCheaperThanLemons newLemonPrice = calcCheaperLimePrice newLemonPrice
      | isFurtherThan50CentsApartFrom newLemonPrice currLimePrice && cannotMakeLimesCheaperThanLemons newLemonPrice = calcMoreExpensiveLimePrice newLemonPrice
      | otherwise = currLimePrice
    isFurtherThan50CentsApartFrom priceA priceB = abs (priceA - priceB ) > 0.5
    canMakeLimesCheaperThanLemons = subtract 0.5 >>>  (>= 0)
    cannotMakeLimesCheaperThanLemons = not . canMakeLimesCheaperThanLemons
    calcCheaperLimePrice = subtract 0.5
    calcMoreExpensiveLimePrice = (+ 0.5)

limePrice' :: Lens' ProducePrices Float
limePrice' = error "x"
