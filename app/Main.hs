{-# LANGUAGE Arrows #-}
module Main where

import Prelude

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Interface.IO.Game as G

import FRP.Yampa
import Control.Monad

import qualified Player as P
import qualified Bullet as B
import qualified Enemy as E
import YampaGloss

main :: IO ()
main =
  playYampa
    (InWindow "Yampa Example" (640, 480) (200, 320))
    white
    30
    (WorldState (P.Output (0.0, 0.0) NoEvent) [])
    mainSF
    (\(WorldState p bs) -> Pictures $ P.draw p : fmap B.draw bs )

destroyBullets :: SF [B.Output] (Event ([B.Bullet] -> [B.Bullet]))
destroyBullets = proc bos -> do
  dEvs <- arr (fmap B.destroy) -< bos
  dEdge <- edge <<< arr (any isEvent) -< dEvs
  ev <- arr (uncurry tagWith) <<< first (arr killMatch) -< (dEvs, dEdge)
  returnA -< ev
  where
    killMatch :: [Event ()] -> [B.Bullet] -> [B.Bullet]
    killMatch [] l = l
    killMatch (Event () : es) (x : xs) = killMatch es xs
    killMatch (NoEvent : es) (x : xs) = x : killMatch es xs

spawnSpiralBullets :: SF () (Event [B.Bullet])
spawnSpiralBullets = proc () -> do
  spawnEdge <- repeatedly 0.1 () -< ()
  c <- hold 0 <<< count -< spawnEdge
  let theta = fromIntegral c * 15.0 / 360.0 * 2.0 * pi
  let v = 30.0 *^ (cos theta, sin theta)
  let bs = [B.simpleBullet (0.0, 0.0) v]
  spawnE <- arr (uncurry tagWith) -< (bs, spawnEdge)
  returnA -< spawnE

bullets :: SF B.Input [B.Output]
bullets = proc i -> do
  spawnE <- spawnSpiralBullets -< ()
  let spawnFunE = fmap (++) spawnE
  rec
    destroyE <- destroyBullets -< bos
    bos <- drpSwitchB [] -< (i, mergeBy (.) spawnFunE destroyE)
  returnA -< bos

data WorldState = WorldState {pst :: P.Output, bsts :: [B.Output]}

mainSF :: SF (Event G.Event) WorldState
mainSF = proc e -> do
  rec
    p <- P.player (0.0, 0.0) -< (P.Input e)
    let enemyInput = E.Input (P.pos p)
    eo <- E.enemy (0.0, 0.0) (0.0, 0.0) 300.0 -< enemyInput
    let ebSpawn = E.bulletSpawn eo
    let spawnFunE = fmap (++) ebSpawn
    bsts <- drpSwitchB [] -< ((), spawnFunE)
    spPressed <- hold False <<< arr (fmap (==G.Down)) <<< arr (>>= filterKey (G.SpecialKey G.KeySpace)) -< e
  returnA -< WorldState p bsts
  where
    filterKey k (G.EventKey k' st _ _) | k == k' = Event st
                                       | otherwise = NoEvent
    filterKey k _ = NoEvent

