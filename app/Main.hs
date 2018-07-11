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
import qualified Stage as S
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

gatherDestroy :: [Event ()] -> Event ([a] -> [a])
gatherDestroy evs | any isEvent evs = Event $ killMatch evs
                  | otherwise = NoEvent
  where
    killMatch :: [Event ()] -> [a] -> [a]
    killMatch [] l = l
    killMatch (Event () : es) (x : xs) = killMatch es xs
    killMatch (NoEvent : es) (x : xs) = x : killMatch es xs

killSpawn :: SF (a, Event [SF a b], [Event ()]) [b]
killSpawn = proc (a, spawned, destroyed) -> do
  bs <- drpSwitchB [] -< (a, mergeBy (.) (fmap (++) spawned) (gatherDestroy destroyed))
  returnA -< bs

killParSpawn :: SF (a, [Event [SF a b]], [Event ()]) [b]
killParSpawn = proc (a, pSpawned, destroyed) -> do
  bs <- drpSwitchB [] -< (a, mergeBy (.) ((++) <$> foldr (mergeBy (++)) NoEvent pSpawned) (gatherDestroy destroyed))
  returnA -< bs

data WorldState = WorldState {pOutput :: P.Output, bOutput :: [B.Output]}

mainSF :: SF (Event G.Event) WorldState
mainSF = proc e -> do
  rec
    so <- S.stage1 -< ()
    p <- P.player (0.0, 0.0) -< (P.Input e)
    let enemyInput = E.Input (P.pos p)
    eos <- killSpawn -< (enemyInput, S.eSpawn so, fmap E.destroy eos)
    bos <- killParSpawn -< ((), fmap E.bulletSpawn eos, fmap B.destroy bos)
  returnA -< WorldState p bos

