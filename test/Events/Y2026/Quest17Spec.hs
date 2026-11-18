module Events.Y2026.Quest17Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 17" $ do
  it "Sample" $ do
    s <- quest17 "sample"
    s `shouldBe` (21, 94, 31)

  it "Actual" $ do
    a <- quest17 "actual"
    a `shouldBe` (184, 6260, 2768)