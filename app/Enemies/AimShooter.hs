{-# LANGUAGE Arrows #-}
module Enemies.AimShooter where

import FRP.Yampa

import qualified Enemy as E
import qualified Bullet as B
import Defs

aimShot :: Time -> Float -> SF (E.Input, Vec2) (Event [B.Bullet])
aimShot interval speed = proc (E.Input pPos, p) -> do
  b <- arr (uncurry (B.aimingBullet speed)) -< (p, pPos)
  e <- Event () --> repeatedly interval () -< ()
  ev <- arr (uncurry tag) -< (e, [b])
  returnA -< ev

