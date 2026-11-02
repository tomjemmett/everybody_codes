module Events.Y2026.Quest01Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 1" $ do
  it "Sample" $ do
    s <- quest01 "sample"
    s `shouldBe` (11, 9, 61)

  it "Actual" $ do
    a <- quest01 "actual"
    a `shouldBe` (1288, 288, 672188)
