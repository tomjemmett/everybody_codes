module Events.Y2026.Quest10Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 10" $ do
  it "Sample" $ do
    s <- quest10 "sample"
    s `shouldBe` (29, 50000, 13314695352588)

  it "Actual" $ do
    a <- quest10 "actual"
    a `shouldBe` (63, 140070, 13999999999632)
