module Events.Y2026.Quest02Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 2" $ do
  it "Sample" $ do
    s <- quest02 "sample"
    s `shouldBe` (122157, 122157, 122157)


  it "Actual" $ do
    a <- quest02 "actual"
    a `shouldBe` (402981, 56858503527, 600096968370)
