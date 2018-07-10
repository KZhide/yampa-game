module EnemySpec (spec) where

import Test.Hspec
import Enemy
import FRP.Yampa.VectorSpace

spec :: Spec
spec = do
  describe "aim" $ do
    it "returns a unit vector that aims target point" $ do
      let src = (20.0, 50.0)
      let tgt = (45.0, 32.0)
      norm (tgt - src) *^ aim src tgt - (tgt - src) `shouldSatisfy` (\v -> norm v < 0.0001)
    it "does not throw error even if src and tgt are nearly close" $ do
      aim (0.0, 0.0) (0.0, 0.0) `shouldBe` (0.0, -1.0)
