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

aimShot :: Time -> Float -> SF (Vec2, Vec2) (Event [B.Bullet])
aimShot t s = proc (ePos, pPos) -> do
  v <- arr ((s *^) . normalize . uncurry (-)) -< (pPos, ePos)
  bs <- arr (return . uncurry B.bullet) -< (ePos, v)
  e <- repeatedly t () -< ()
  ev <- arr (uncurry tag) -< (e, bs)
  returnA -< ev

enemy :: Vec2 -> Vec2 -> Time -> Enemy
enemy p0 v0 stay = proc i -> do
  v <- constant v0 -< ()
  dp <- integral -< v
  p <- arr (uncurry (+)) <<< constant p0 &&& arr id -< dp
  e <- edge <<< arr (>= stay) <<< time -< ()
  ebs <- aimShot 0.3 10.0 <<< second (arr pPos) -< (p, i)
  returnA -< Output p ebs e
  where
    outOfRealm (x, y) = x > 100.0 || x < -100.0 || y > 100.0 || y < -100.0

draw :: Output -> Picture
draw o = uncurry Translate (pos o) $ Circle 4.0
