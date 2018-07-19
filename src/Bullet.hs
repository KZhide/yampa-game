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

data Output = Output {state :: ObjState, spawn :: Event [Bullet], destroy :: Event ()}
type Bullet = SF ObjInput Output

aim :: Vec2 -> Vec2 -> Vec2
aim src dst | norm (dst ^-^ src) < 0.0001 = vector2 0.0 (-1.0)
            | otherwise = normalize (dst ^-^ src)

bullet :: SF ObjInput ObjState -> SF (ObjInput, ObjState) (Event [Bullet]) -> SF (ObjInput, ObjState) (Event ()) -> Bullet
bullet posSF spawnSF destroySF = proc i -> do
  p <- posSF -< i
  spEv <- spawnSF -< (i, p)
  dEv <- destroySF -< (i, p)
  returnA -< Output p spEv dEv

aimingBullet :: Float -> (ObjInput, ObjState) -> Bullet
aimingBullet speed (ObjInput (PlayerPos pp), ObjState{v, p}) =
  simpleBullet ObjState{v = speed *^ aim p pp, p = p}

simpleBullet :: ObjState -> Bullet
simpleBullet st = bullet (move st >>> arr fst) never (arr snd >>> outOfArea (-100.0) 100.0 100.0 (-100.0))

draw :: Output -> Picture
draw o = (uncurry Translate . vector2XY . p . state) o $ Circle 4.0
