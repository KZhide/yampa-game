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
import ObjInput
import Stage
import YampaGloss

import Stages.Stage1

main :: IO ()
main =
  playYampa
    (InWindow "Yampa Example" (640, 480) (200, 320))
    white
    30
    (WorldState (P.Output (PlayerPos zeroVector) NoEvent) [] [])
    mainSF
    (\(WorldState p bos eos) -> Pictures $ P.draw p : fmap B.draw bos ++ fmap E.draw eos)

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

data WorldState = WorldState {pOutput :: P.Output, bOutput :: [B.Output], eOutput :: [E.Output]}

mainSF :: SF (Event G.Event) WorldState
mainSF = proc e -> do
  so <- stage1 -< ()
  p <- P.player zeroVector -< (P.Input e)
  let objInput = ObjInput (P.pos p)
  rec
    eos <- killSpawn -< (objInput, eSpawn so, fmap (void.snd) eos)
    bos <- killParSpawn -< (objInput, fmap (E.bulletSpawn.fst) eos ++ fmap (B.spawn.fst) bos, fmap (void.snd) bos)
  returnA -< WorldState p (fmap fst bos) (fmap fst eos)

