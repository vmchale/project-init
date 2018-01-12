import           Lib
import           Test.Hspec

main :: IO ()
main = hspec $
    describe "head'" $
        parallel $ it "gets the head of an infinite list" $
            head' [1..] `shouldBe` (Just 1 :: Maybe Integer)
