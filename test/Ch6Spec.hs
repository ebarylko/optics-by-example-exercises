module Ch6Spec (spec) where
import Ch_6(numOfDaysUntilFirstThaw,
           warmestTempInFirstFourDays,
           nextTempAfterWarmestTempInFirstFourDays,
           numOfConsecDaysOfBelowFreezingWeather,
           tempsFromFirstThawToNextFreeze,
           tempsFromFirstThawToFinalFreeze,
           Card(..),
           Aura(..),
           Move(..),
           namesThatStartWithS)

import Test.Hspec

tempMeasurements :: [Int]
tempMeasurements = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

deck :: [Card]
deck = [ Card "Skwortul" Wet False [Move"Squirt" 20],
         Card "Scorchander" Hot False [Move"Scorch" 20],
         Card "Seedasaur" Leafy False [Move"Allergize" 20],
         Card "Kapichu" Spark False [Move"Poke" 10 , Move"Zap" 30],
         Card "Elecdude" Spark False [Move"Asplode" 50],
         Card "Garydose" Wet True [Move"Gary's move" 40],
         Card "Moisteon" Wet False [Move"Soggy" 3],
         Card "Grasseon" Leafy False [Move"Leaf Cut" 30],
         Card "Spicyeon" Hot False [Move"Capsaicisize" 40],
         Card "Sparkeon" Spark True [Move"Shock" 40 , Move"Battery" 50]]

spec :: Spec

spec = do
  describe "Calculating the number of days until the first thaw" $ do
    it "Should be two days" $ do
      numOfDaysUntilFirstThaw tempMeasurements `shouldBe` 2

  describe "Calculating the warmest temperature in the first four measurements" $ do
    it "Should be four degrees" $ do
      warmestTempInFirstFourDays tempMeasurements `shouldBe` Just 4

  describe "Calculating the temperature next to the warmest temperature in the first four measurements" $ do
    it "Should be three degrees" $ do
      nextTempAfterWarmestTempInFirstFourDays tempMeasurements `shouldBe` Just 3

  describe "Calculating the number of consecutive days with freezing weather at the end of the sample" $ do
    it "Should be two days" $ do
      numOfConsecDaysOfBelowFreezingWeather tempMeasurements `shouldBe` 2

  describe "Extracting the temperatures between the first thaw and subsequent freeze" $ do
    it "Should contain four samples" $ do
      tempsFromFirstThawToNextFreeze tempMeasurements `shouldBe` [4, 3, 8, 6]

  describe "Extracting the temperatures between the first thaw and final freeze" $ do
    it "Should contain the samples from the third day onward to the eighth day" $ do
      tempsFromFirstThawToFinalFreeze tempMeasurements `shouldBe` [4, 3, 8, 6, -2, 3]


  describe "Extracting the cards whose names start with s" $ do
    it "Should only return the first three names and the last two names" $ do
      namesThatStartWithS deck `shouldBe` ["Skwortul", "Scorchander", "Seedasaur", "Spicyeon", "Sparkeon"]
