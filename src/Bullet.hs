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

bullet :: SF Input Vec2 -> SF (Input, Vec2) (Event ()) -> Bullet
bullet posSF destroySF = proc i -> do
  p <- posSF -< i
  dEv <- destroySF -< (i, p)
  returnA -< Output p dEv

aimingBullet :: Float -> Vec2 ->  Vec2 -> Bullet
aimingBullet speed src dst = simpleBullet src (speed *^ aim src dst)

simpleBullet :: Vec2 -> Vec2 -> SF Input Output
simpleBullet v p0 = bullet (move v p0 >>> arr fst) (arr snd >>> outOfArea (-100.0) 100.0 100.0 (-100.0))

draw :: Output -> Picture
draw o = (uncurry Translate . vector2XY . pos) o $ Circle 4.0
