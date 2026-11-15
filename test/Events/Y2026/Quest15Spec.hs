module Events.Y2026.Quest15Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 15" $ do
  it "Sample" $ do
    s <- quest15 "sample"
    s `shouldBe` (184, 5448, 540320698784)

  it "Actual" $ do
    a <- quest15 "actual"
    a `shouldBe` (233, 20321, 391266712921)
