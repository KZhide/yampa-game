{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}

module Bullet where
import Defs
import Graphics.Gloss
import FRP.Yampa
import FRP.Yampa.Vector2
import qualified Graphics.Gloss.Interface.IO.Game as G
import Shared
import ObjInput
import Control.Monad

data Output = Output {pos :: Vec2, spawn :: Event [Bullet]}
type Bullet = SF ObjInput (Output, Event Vec2)

aim :: Vec2 -> Vec2 -> Vec2
aim src dst | norm (dst ^-^ src) < 0.0001 = vector2 0.0 (-1.0)
            | otherwise = normalize (dst ^-^ src)

bullet :: SFState ObjInput Vec2 ()
  -> SFState (ObjInput, Vec2) (Event [Bullet]) ()
  -> SFState (ObjInput, Vec2) () () -> Vec2 -> Bullet
bullet posSF spawnSF destroySF p0 = proc i -> do
  (p, evA) <- runSFState posSF p0 -< i
  (spEv, evB) <- runSFState spawnSF NoEvent -< (i, p)
  (_, dEv) <- runSFState destroySF () -< (i, p)
  returnA -< (Output p spEv, (void evA `lMerge` void dEv) `tag` p)

aimingBullet :: Float -> (ObjInput, Vec2) -> Bullet
aimingBullet speed (ObjInput (PlayerPos pp), p) =
  simpleBullet (speed *^ aim p pp) p

simpleBullet :: Vec2 -> Vec2 -> Bullet
simpleBullet v =
  bullet
    (move v)
    noReturn
    (sfstate (\_ -> arr (outOfArea . snd) >>> (constant () &&& edge)))

draw :: Output -> Picture
draw o = (uncurry Translate . vector2XY . pos) o $ Circle 4.0
