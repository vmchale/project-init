import           Lib
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "head" $ do
        parallel $ it "parses a .mad string with modifiers" $ do
            head [1..] `shouldBe` 1
