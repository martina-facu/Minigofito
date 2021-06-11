import PdePreludat
import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Test de ejemplo" $ do
    it "Descripción del test" $ do
      id 1 `shouldBe` 1

