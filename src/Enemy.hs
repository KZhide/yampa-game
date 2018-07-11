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
  b <- arr (uncurry (B.aimingBullet s)) -< (ePos, pPos)
  e <- repeatedly t () -< ()
  ev <- arr (uncurry tag) -< (e, [b])
  returnA -< ev

spiralShot :: Time -> Float -> Float -> Float -> SF Vec2 (Event [B.Bullet])
spiralShot span speed dTheta theta0 = proc p -> do
  spawnEdge <- repeatedly span () -< ()
  c <- hold 0 <<< count -< spawnEdge
  let theta = fromIntegral c * dTheta + theta0
  let v = speed *^ (cos theta, sin theta)
  bs <- arr (return . uncurry B.simpleBullet) -< (p, v)
  spawnE <- arr (uncurry tagWith) -< (bs, spawnEdge)
  returnA -< spawnE

enemy :: Vec2 -> Vec2 -> Time -> Enemy
enemy p0 v0 stay = proc i -> do
  v <- constant v0 -< ()
  dp <- integral -< v
  p <- arr (uncurry (+)) <<< constant p0 &&& arr id -< dp
  e <- edge <<< arr (>= stay) <<< time -< ()
  ebs <- aimShot 0.8 50.0 <<< second (arr pPos) -< (p, i)
  returnA -< Output p ebs e

draw :: Output -> Picture
draw o = uncurry Translate (pos o) $ Circle 4.0
