module Events.Y2026.Quest12Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 12" $ do
  it "Sample" $ do
    s <- quest12 "sample"
    s `shouldBe` (7, 6, 16)

  it "Actual" $ do
    a <- quest12 "actual"
    a `shouldBe` (208, 1972, 2853)
