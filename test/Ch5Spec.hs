module Ch5Spec (spec) where
import Ch_5(Gate(..),
            Army(..),
            Kingdom(..),
            lens1,
            lens2,
            lens3)

import Test.Hspec

duloc :: Kingdom
duloc = Kingdom { _name= "Duloc" ,
                  _army = Army { _archers= 22 , _knights= 14} ,
                  _gate= Gate { _open= True , _oilTemp= 10.0}}

spec :: Spec

spec = do
  describe "Chapter five exercises" $ do
    it "Applying the first lens has the intended effect" $ do
      let expected = Kingdom { _name= "Duloc: a perfect place" ,
                               _army = Army { _archers= 22 , _knights= 42} ,
                               _gate= Gate { _open= False , _oilTemp= 10.0}}
      lens1 duloc `shouldBe` expected

    it "Applying the second lens has the intended effect" $ do
      let expected = Kingdom { _name= "Dulocinstein" ,
                               _army = Army { _archers= 17 , _knights= 26} ,
                               _gate= Gate { _open= True , _oilTemp= 100.0}}
      lens2 duloc `shouldBe` expected

    it "Applying the third lens has the intended effect" $ do
      let expected = ( "Duloc: Home" ,
                       Kingdom { _name= "Duloc: Home of the talking Donkeys"
                               , _army = Army { _archers= 22 , _knights= 14}
                               , _gate= Gate { _open= True , _oilTemp= 5.0}})
      lens3 duloc `shouldBe` expected
