module Events.Y2026.Quest13Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 13" $ do
  it "Sample" $ do
    s <- quest13 "sample"
    s `shouldBe` (9, 21, 21)

  it "Actual" $ do
    a <- quest13 "actual"
    a `shouldBe` (28, 136282, 1091739590408)
