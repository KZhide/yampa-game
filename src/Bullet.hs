{-# LANGUAGE Arrows #-}

module Bullet where
import Defs
import Graphics.Gloss
import FRP.Yampa
import FRP.Yampa.Vector2
import qualified Graphics.Gloss.Interface.IO.Game as G
import Shared

type Input = ()
data Output = Output {pos :: Vec2, destroy :: Event ()} deriving (Eq, Show)
type Bullet = SF Input Output

aim :: Vec2 -> Vec2 -> Vec2
aim src dst | norm (dst ^-^ src) < 0.0001 = vector2 0.0 (-1.0)
            | otherwise = normalize (dst ^-^ src)

aimingBullet :: Float -> Vec2 ->  Vec2 -> Bullet
aimingBullet speed src dst = simpleBullet src (speed *^ aim src dst)

simpleBullet :: Vec2 -> Vec2 -> SF Input Output
simpleBullet p0 v0 = proc () -> do
  p <- imIntegral p0 <<< constant v0 -< ()
  e <- outOfArea (-100.0) 100.0 100.0 (-100.0) -< p
  returnA -< Output p e

draw :: Output -> Picture
draw o = (uncurry Translate . vector2XY . pos) o $ Circle 4.0
