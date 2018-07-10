{-# LANGUAGE Arrows #-}

module Bullet where
import Defs
import Graphics.Gloss
import FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

type Input = ()
data Output = Output {pos :: Vec2, destroy :: Event ()} deriving (Eq, Show)
type Bullet = SF Input Output

aim :: Vec2 -> Vec2 -> Vec2
aim src dst | norm (dst - src) < 0.0001 = (0.0, -1.0)
            | otherwise = normalize (dst - src)

aimingBullet :: Float -> Vec2 ->  Vec2 -> Bullet
aimingBullet speed src dst = simpleBullet src (speed *^ aim src dst)

simpleBullet :: Vec2 -> Vec2 -> SF Input Output
simpleBullet p0 v0 = proc () -> do
  v <- constant v0 -< ()
  dp <- integral -< v
  p <- arr (uncurry (+)) <<< constant p0 &&& arr id -< dp
  e <- edge <<< arr outOfRealm -< p
  returnA -< Output p e
  where
    outOfRealm (x, y) = x > 100.0 || x < -100.0 || y > 100.0 || y < -100.0

draw :: Output -> Picture
draw o = uncurry Translate (pos o) $ Circle 4.0
