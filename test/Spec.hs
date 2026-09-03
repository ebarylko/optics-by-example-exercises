import Test.Hspec
import Ch_3(fullName,
            User(..))
import Control.Lens

defaultUser = User "John" "Cena" "invisible@example.com"

main :: IO ()
main = hspec $ do
  describe "viewing the full name of a user" $ do
    it "is the concatenation of their first and last name" $ do
      view fullName defaultUser `shouldBe` "John Cena"

  describe "Setting a new full name of a user of the form firstName remainingPart" $ do
    it "sets the new first name as firstName and the last name as remainingPart" $ do
      set fullName "Doctor of Thuganomics" defaultUser `shouldBe` defaultUser
