module Events.Y2026.Quest03Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 3" $ do
  it "Sample" $ do
    s <- quest03 "sample"
    s `shouldBe` (1478, 1161463, 50987)

  it "Actual" $ do
    a <- quest03 "actual"
    a `shouldBe` (18427, 25483144305, 334855)
