import           Lib
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "head" $ do
        parallel $ it "gets the head of an infinite list" $ do
            head [1..] `shouldBe` 1
