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

data Output = Output {state :: ObjState, spawn :: Event [Bullet]}
type Bullet = SF ObjInput (Output, Event ObjState)

aim :: Vec2 -> Vec2 -> Vec2
aim src dst | norm (dst ^-^ src) < 0.0001 = vector2 0.0 (-1.0)
            | otherwise = normalize (dst ^-^ src)

bullet :: SF ObjInput (ObjState, Event a) -> SF (ObjInput, ObjState) (Event [Bullet], Event b) -> SF (ObjInput, ObjState) (Event c) -> Bullet
bullet posSF spawnSF destroySF = proc i -> do
  (st, evA) <- posSF -< i
  (spEv, evB) <- spawnSF -< (i, st)
  dEv <- destroySF -< (i, st)
  returnA -< (Output st spEv, (void evA `lMerge` void dEv) `tag` st)

aimingBullet :: Float -> (ObjInput, ObjState) -> Bullet
aimingBullet speed (ObjInput (PlayerPos pp), ObjState{v, p}) =
  simpleBullet ObjState{v = speed *^ aim p pp, p = p}

simpleBullet :: ObjState -> Bullet
simpleBullet st = bullet (move st) (never &&& never) (arr (outOfArea . snd) >>> edge)

draw :: Output -> Picture
draw o = (uncurry Translate . vector2XY . p . state) o $ Circle 4.0
