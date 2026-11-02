module Events.Y2026.Quest03Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 3" $ do
  it "Sample" $ do
    s <- quest03 "sample"
    s `shouldBe` (0, 0, 0)


  it "Actual" $ do
    a <- quest03 "actual"
    a `shouldBe` (0, 0, 0)
