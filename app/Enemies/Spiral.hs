{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
module Enemies.Spiral where

import FRP.Yampa

import qualified Enemy as E
import qualified Bullet as B
import Defs

data Args = Args {
  interval :: Time,
  speed :: Float,
  omega :: Float,
  theta0 :: Float
} deriving (Eq, Show)

spiral :: Args -> SF (E.Input, Vec2) (Event [B.Bullet])
spiral Args{interval, speed, omega, theta0} = proc (_, p) -> do
  spawnEdge <- repeatedly interval () -< ()
  c <- hold 0 <<< count -< spawnEdge
  let theta = fromIntegral c * omega + theta0
  let v = speed *^ (cos theta, sin theta)
  bs <- arr (return . uncurry B.simpleBullet) -< (p, v)
  spawnE <- arr (uncurry tagWith) -< (bs, spawnEdge)
  returnA -< spawnE

