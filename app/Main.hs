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

data WorldState = WorldState {pst :: P.Output, bsts :: [B.Output]}

mainSF :: SF (Event G.Event) WorldState
mainSF = proc e -> do
  rec
    bst <- B.bullet (0.0, 0.0) (10.0, 0.0) -< ()
    p <- P.player (0.0, 0.0) -< (P.Input e)
    spPressed <- hold False <<< arr (fmap (==G.Down)) <<< arr (>>= filterKey (G.SpecialKey G.KeySpace)) -< e
  returnA -< WorldState p [bst]
  where
    filterKey k (G.EventKey k' st _ _) | k == k' = Event st
                                       | otherwise = NoEvent
    filterKey k _ = NoEvent

