{-# LANGUAGE Arrows #-}

module Bullet where
import Defs
import Graphics.Gloss
import FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

type Input = ()
data Output = Output {pos :: Vec2, destroy :: Event ()} deriving (Eq, Show)
type Bullet = SF Input Output

bullet :: Vec2 -> Vec2 -> SF Input Output
bullet p0 v0 = proc () -> do
  v <- constant v0 -< ()
  dp <- integral -< v
  p <- arr (uncurry (+)) <<< constant p0 &&& arr id -< dp
  e <- edge <<< arr outOfRealm -< p
  returnA -< Output p e
  where
    outOfRealm (x, y) = x > 100.0 || x < -100.0 || y > 100.0 || y < -100.0

draw :: Output -> Picture
draw o = uncurry Translate (pos o) $ Circle 4.0
