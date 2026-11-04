module Events.Y2026.Quest07Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 7" $ do
  it "Sample" $ do
    s <- quest07 "sample"
    s `shouldBe` (102, 1316, 187)

  it "Actual" $ do
    a <- quest07 "actual"
    a `shouldBe` (257, 128010029, 32938855)
