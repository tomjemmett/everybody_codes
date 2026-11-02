module Events.Y2026.Quest04Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 4" $ do
  it "Sample" $ do
    s <- quest04 "sample"
    s `shouldBe` (27598, 6470, 3004)

  it "Actual" $ do
    a <- quest04 "actual"
    a `shouldBe` (173627, 582121862, 6452929)
