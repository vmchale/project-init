module Test.Spec

import Specdris.Spec

export

specSuite : IO ()
specSuite = spec $ do
  describe "something trivial" $
    it "should add two numbers correctly" $
      14 + 3 `shouldBe` 17
