module Events.Y2026.Quest06Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 6" $ do
  it "Sample" $ do
    s <- quest06 "sample"
    s `shouldBe` (7, 6, 3)

  it "Actual" $ do
    a <- quest06 "actual"
    a `shouldBe` (45, 839, 1646)
