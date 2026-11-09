module Stories.S4.Quest01Spec (spec) where

import SpecHelper
import Stories.S4.Quests

spec :: Spec
spec = describe "Story 4, Quest 1" $ do
  it "Sample" $ do
    s <- quest01 "sample"
    s `shouldBe` (66, 43, 35)

  it "Actual" $ do
    a <- quest01 "actual"
    a `shouldBe` (287, 32647, 39526)
