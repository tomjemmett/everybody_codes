module Events.Y2026.Quest08Spec (spec) where

import Control.Monad (when)
import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 8" $ do
  it "Sample" $ do
    s <- quest08 "sample"
    s `shouldBe` (32, 187, 899889)

  it "Actual" $ do
    a <- quest08 "actual"
    a `shouldBe` (173, 1024, 899250)
