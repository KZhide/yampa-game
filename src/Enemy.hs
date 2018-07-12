{-# LANGUAGE Arrows #-}

module Enemy where
import Defs
import Graphics.Gloss
import FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Bullet as B

data Input = Input {
  pPos :: Vec2
}

data Output = Output {
  pos :: Vec2,
  bulletSpawn :: Event [B.Bullet],
  destroy :: Event ()
}

type Enemy = SF Input Output

line :: Vec2 -> Vec2 -> Time -> SF Input Vec2
line p0 p1 duration = proc _ -> do
  dPos <- integral -< realToFrac (1.0 / duration) *^ (p1 - p0)
  returnA <<< arr (uncurry (+)) -< (p0, dPos)

enemy :: SF Input Vec2 -> SF (Input, Vec2) (Event [B.Bullet]) -> SF (Input, Vec2) (Event ()) -> Enemy
enemy posSF bSpawnSF destroySF = proc i -> do
  p <- posSF -< i
  bsEv <- bSpawnSF -< (i, p)
  dEv <- destroySF -< (i, p)
  returnA -< Output p bsEv dEv

draw :: Output -> Picture
draw o = uncurry Translate (pos o) $ Color blue $ Circle 8.0
