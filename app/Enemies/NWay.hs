{-# LANGUAGE Arrows #-}
module Enemies.NWay where

import FRP.Yampa
import FRP.Yampa.Vector2

import qualified Enemy as E
import qualified Bullet as B
import Defs

nway :: Int -> Float -> Vec2 -> Vec2 -> [B.Bullet]
nway n angle v p =
  [B.simpleBullet (vector2Rotate (angle * (fromIntegral m/(fromIntegral n-1.0) - 0.5) / 2.0) v) p | m <- [0..n-1]]
