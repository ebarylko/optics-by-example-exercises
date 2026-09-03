module Ch3Spec (spec) where

import Ch_3(fullName,
            User(..),
            ProducePrices(..),
            lemonPrice,
            limePrice)
import Control.Lens
import Test.Hspec


defaultUser :: User
defaultUser = User "John" "Cena" "invisible@example.com"

prices :: ProducePrices
prices = ProducePrices 1.50 1.48

spec :: Spec
spec =  do
  describe "viewing the full name of a user" $ do
    it "is the concatenation of their first and last name" $ do
      view fullName defaultUser `shouldBe` "John Cena"

  describe "Setting a new full name of a user of the form firstName remainingPart" $ do
    it "sets the new first name as firstName and the last name as remainingPart" $ do
      set fullName "Doctor of Thuganomics" defaultUser `shouldBe` User "Doctor" "of Thuganomics" "invisible@example.com"

  describe "Setting the price of a lime or a lemon to a negative value" $ do
    it "results in setting the corresponding price to zero" $ do
      set lemonPrice (-1) prices `shouldBe` ProducePrices  1.50 0
