{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
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


-- Takes a lens for the product whose price is being adjusted, the lens for the
-- other product, and generates a lens that adjusts the price of the first product while
-- making sure the price of the second product is within fifty cents of the new price
productPrice :: Lens' ProducePrices Float -> Lens' ProducePrices Float -> Lens' ProducePrices Float
productPrice principleProductPrice secondaryProductPrice = lens (view principleProductPrice) adjustSecondaryProductPrice
  where
    adjustSecondaryProductPrice currPrices newPrice = set principleProductPrice newPrice currPrices & set secondaryProductPrice (calcNewSecondaryProductPrice (toNonNegVal newPrice) (view secondaryProductPrice currPrices))
    calcNewSecondaryProductPrice newPrincipleProductPrice = min (newPrincipleProductPrice + 0.5) >>> max (newPrincipleProductPrice - 0.5)

-- Gets the current prices of lemons/limes and
-- adjusts the prices of both such that they are always nonnegative and
-- never more than 50 cents apart
lemonPrice' :: Lens' ProducePrices Float

limePrice' :: Lens' ProducePrices Float

limePrice' = productPrice limePrice lemonPrice
lemonPrice' = productPrice lemonPrice limePrice
