{-# LANGUAGE Arrows #-}
module Bullets.NWay where

import FRP.Yampa
import FRP.Yampa.Vector2

import qualified Bullet as B
import Defs
import ObjInput

nway :: Int -> Float -> Vec2 -> (ObjInput, Vec2) -> [B.Bullet]
nway n angle v0 (_, p) =
  [
    B.simpleBullet
      (vector2Rotate ((angle * 2.0 * pi / 360.0) * (fromIntegral m/(fromIntegral n-1.0) - 0.5) / 2.0) v0)
      p 
    | m <- [0..n-1]
  ]

aimNWay :: Int -> Float -> Float -> (ObjInput, Vec2) -> [B.Bullet]
aimNWay n angle speed (i@(ObjInput (PlayerPos pPos)), p) = nway n angle (speed *^ B.aim p pPos) (i, p)

allWay :: Int -> Vec2 -> (ObjInput, Vec2) -> [B.Bullet]
allWay n v (_, p) =
  [
    B.simpleBullet
      (vector2Rotate (2.0 * pi * fromIntegral m / fromIntegral n) v)
      p
    | m <- [0..n-1]
  ]

aimedAllWay :: Int -> Float -> (ObjInput, Vec2) -> [B.Bullet]
aimedAllWay n speed (i@(ObjInput (PlayerPos pPos)), p) = allWay n (speed *^ B.aim p pPos) (i, p)
