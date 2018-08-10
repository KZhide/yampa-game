{-# LANGUAGE Arrows #-}
module Enemies.AimShooter where

import FRP.Yampa

import qualified Enemy as E
import qualified Bullet as B
import Defs
import ObjInput
import SFState

repeatedAimShot :: Time -> Float -> SFState (ObjInput, Vec2) (Event [B.Bullet]) ()
repeatedAimShot interval speed = do
  oneshot (return . B.aimingBullet speed)
  delayCalc interval ()
  repeatedAimShot interval speed

