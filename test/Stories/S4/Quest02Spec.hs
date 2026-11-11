module Stories.S4.Quest02Spec (spec) where

import SpecHelper
import Stories.S4.Quests

spec :: Spec
spec = describe "Story 4, Quest 2" $ do
  it "Sample" $ do
    s <- quest02 "sample"
    s `shouldBe` (8, 25, 42)

  it "Actual" $ do
    a <- quest02 "actual"
    a `shouldBe` (63, 1683, 15841)
