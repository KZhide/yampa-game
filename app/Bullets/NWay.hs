{-# LANGUAGE Arrows #-}
module Bullets.NWay where

import FRP.Yampa
import FRP.Yampa.Vector2

import qualified Bullet as B
import Defs
import ObjInput

nway :: Int -> Float -> Vec2 -> (ObjInput, ObjState) -> [B.Bullet]
nway n angle v0 (_, ObjState{p = p}) =
  [
    B.simpleBullet ObjState {
      p = p,
      v = vector2Rotate ((angle * 2.0 * pi / 360.0) * (fromIntegral m/(fromIntegral n-1.0) - 0.5) / 2.0) v0
    } 
    | m <- [0..n-1]
  ]

aimNWay :: Int -> Float -> Float -> (ObjInput, ObjState) -> [B.Bullet]
aimNWay n angle speed (i@(ObjInput (PlayerPos pPos)), st@ObjState{p=p}) = nway n angle (speed *^ B.aim p pPos) (i, st)

allWay :: Int -> Vec2 -> (ObjInput, ObjState) -> [B.Bullet]
allWay n v (_, ObjState{p = p}) =
  [
    B.simpleBullet ObjState{
      p = p,
      v = vector2Rotate (2.0 * pi * fromIntegral m / fromIntegral n) v
    }
    | m <- [0..n-1]
  ]

aimedAllWay :: Int -> Float -> (ObjInput, ObjState) -> [B.Bullet]
aimedAllWay n speed (i@(ObjInput (PlayerPos pPos)), st@ObjState{p=p}) = allWay n (speed *^ B.aim p pPos) (i, st)
