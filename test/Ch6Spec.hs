module Ch6Spec (spec) where
import Ch_6(numOfDaysUntilFirstThaw,
           warmestTempInFirstFourDays)

import Test.Hspec

tempMeasurements :: [Int]
tempMeasurements = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

spec :: Spec

spec = do
  describe "Calculating the number of days until the first thaw" $ do
    it "Should be two days" $ do
      numOfDaysUntilFirstThaw tempMeasurements `shouldBe` 2

  describe "Calculating the warmest temperature in the first four measurements" $ do
    it "Should be four degrees" $ do
      warmestTempInFirstFourDays tempMeasurements `shouldBe` Just 4
