import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "head" $ do
        parallel $ it "finds the head of a list" $ do
            head [1..] `shouldBe` 1
