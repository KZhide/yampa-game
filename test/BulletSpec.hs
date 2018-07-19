module BulletSpec (spec) where

import Test.Hspec
import Bullet
import FRP.Yampa.VectorSpace
import FRP.Yampa.Vector2

spec :: Spec
spec =
  describe "aim" $ do
    it "returns a unit vector that aims target point" $ do
      let src = vector2 20.0 50.0
      let tgt = vector2 45.0 32.0
      norm (tgt ^-^ src) *^ aim src tgt ^-^ (tgt ^-^ src) `shouldSatisfy` (\v -> norm v < 0.0001)
    it "does not throw error even if src and tgt are nearly close" $ do
      aim zeroVector zeroVector `shouldBe` vector2 0.0 (-1.0)
