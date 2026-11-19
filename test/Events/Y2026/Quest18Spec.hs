module Events.Y2026.Quest18Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 18" $ do
  it "Sample" $ do
    s <- quest18 "sample"
    s `shouldBe` (96, 205, 205)

  it "Actual" $ do
    a <- quest18 "actual"
    a `shouldBe` (726, 2383424, 930062034830201)
