module Events.Y2024.Quest15Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 15" $ do
  it "Sample" $ do
    s <- quest15 "sample"
    s `shouldBe` (26, 38, 0)

  it "Actual" $ do
    a <- quest15 "actual"
    a `shouldBe` (180, 496, 1560)
