{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
module Enemies.Spiral where

import FRP.Yampa

import qualified Enemy as E
import qualified Bullet as B
import Defs
import ObjInput
import SFState
import Control.Monad (void)
import Shared

data Args = Args {
  interval :: Time,
  speed :: Float,
  omega :: Float,
  theta0 :: Float
} deriving (Eq, Show)

spiral :: Args -> SFState (ObjInput, Vec2) (Event [B.Bullet]) ()
spiral args = void (infBind (return 0) (spiral' args))
  where
    infBind :: Monad m => m a -> (a -> m a) -> m a
    infBind m f = do
      o <- m
      infBind (f o) f
    spiral' :: Args -> Int -> SFState (ObjInput, Vec2) (Event [B.Bullet]) Int
    spiral' Args{interval, speed, omega, theta0} n = do
      let theta = fromIntegral n * omega + theta0
      let v = speed *^ (cos theta, sin theta)
      oneShot (B.simpleBullet v)
      delayCalc interval (n+1)

