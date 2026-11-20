module Events.Y2026.Quest19Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 19" $ do
  it "Sample" $ do
    s <- quest19 "sample"
    s `shouldBe` (144, 180, 8000)

  it "Actual" $ do
    a <- quest19 "actual"
    a `shouldBe` (2813940, 278279608320, 2055003004548)