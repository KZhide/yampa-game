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

autoBullets :: SF B.Input [B.Output]
autoBullets = proc i -> do
  spawnEdge <- repeatedly 0.1 () -< i
  c <- hold 0<<< count -< spawnEdge
  spawnE <- arr (\(c, spawnEdge) -> spawnEdge `tag` (\bs -> bs ++ [B.bullet (0.0, 0.0) (30.0 * cos(fromIntegral c * 15.0 / 360.0 * 2.0 * pi), 30.0 * sin(fromIntegral c * 15.0 / 360.0 * 2.0 * pi))])) -< (c, spawnEdge)
  rec
    dList <- arr (fmap B.destroy) -< bos
    dEdge <- edge <<< arr (any isEvent) -< dList
    destroyE <- arr (\(l, e) -> tag e (killMatch l)) <<< first (arr (fmap B.destroy)) -< (bos, dEdge)
    bos <- drpSwitchB [] -< (i, mergeBy (.) spawnE destroyE)
  returnA -< bos
  where
    killMatch :: [Event ()] -> [B.Bullet] -> [B.Bullet]
    killMatch [] l = l
    killMatch (Event () : es) (x : xs) = killMatch es xs
    killMatch (NoEvent : es) (x : xs) = x : killMatch es xs

data WorldState = WorldState {pst :: P.Output, bsts :: [B.Output]}

mainSF :: SF (Event G.Event) WorldState
mainSF = proc e -> do
  rec
    bsts <- autoBullets -< ()
    p <- P.player (0.0, 0.0) -< (P.Input e)
    spPressed <- hold False <<< arr (fmap (==G.Down)) <<< arr (>>= filterKey (G.SpecialKey G.KeySpace)) -< e
  returnA -< WorldState p bsts
  where
    filterKey k (G.EventKey k' st _ _) | k == k' = Event st
                                       | otherwise = NoEvent
    filterKey k _ = NoEvent

