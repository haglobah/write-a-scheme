module TestSuite where

import Main
import Test.Hspec
import Test.QuickCheck

scheme :: String -> String
scheme code = fmap show (readExpr code >>= eval)

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    it "atoms work" $ do
      scheme "'hello" `shouldBe` "'hello"