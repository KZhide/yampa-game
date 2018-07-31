{-# LANGUAGE Arrows #-}
--module Player (PlayerState, player, playerVelocity, drawEntity) where
module Player (PlayerPos, player, Input(Input), Output(..), draw) where

import FRP.Yampa
import FRP.Yampa.VectorSpace
import FRP.Yampa.Vector2
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import Defs
import ObjInput
import YampaGlossInterface
import qualified Bullet as B

data Input = Input (Event G.Event) deriving (Eq, Show)
data Output = Output {pos :: PlayerPos, shot :: Event [B.Bullet]}

type Player = SF Input Output

keyOnOff :: G.Key -> SF (Event G.Event) (Event Bool)
keyOnOff = arr . keyEvent
  where
    keyEvent k (Event (G.EventKey k' s _ _)) | k == k' = Event (s == G.Down)
                                             | otherwise = NoEvent
    keyEvent k _ = NoEvent

bulletSpawn :: SF (PlayerPos, Event G.Event) (Event [B.Bullet])
bulletSpawn = proc (p, e) -> do
  isShotKeyOn <- hold False <<< keyOnOff (G.SpecialKey G.KeySpace) -< e
  returnA  <<< shotSF -< (p, isShotKeyOn)
  where
    shotSF :: SF (PlayerPos, Bool) (Event [B.Bullet])
    shotSF = proc (PlayerPos p, b) -> do
      let bs = [B.simpleBullet (vector2 0.0 30.0) p]
      ev <- repeatedly 0.1 () -< ()
      bsEv <- arr (uncurry tagWith) -< (bs, ev)
      returnA -< if b then bsEv else NoEvent

player :: Vec2 -> Player
player p0 = proc (Input e) -> do
  v <- playerVelocity -< e
  p <- arr PlayerPos <<< integral -< v
  sp <- bulletSpawn -< (p, e)
  --bs <- arr (uncurry tag) <<< repeatedly 1.0 () &&& arr ((:[]). uncurry B.bullet) -< (p, v)
  returnA -< Output p sp

playerVelocity :: SF (Event G.Event) Vec2
playerVelocity = (constant zeroVector &&& arr pvChange) >>> impulseIntegral

draw :: Output -> Picture
draw (Output (PlayerPos p) _) = transV2 p $ color red (Circle 12.0)

direction :: G.SpecialKey -> Vec2
direction G.KeyUp = vector2 0.0 100.0
direction G.KeyDown = vector2 0.0 (-100.0)
direction G.KeyLeft = vector2 (-100.0) 0.0
direction G.KeyRight = vector2 100.0 0.0
direction _ = zeroVector

pvChange :: Event G.Event -> Event Vec2
pvChange (Event (G.EventKey (G.SpecialKey k) G.Down _ _)) = Event (direction k)
pvChange (Event (G.EventKey (G.SpecialKey k) G.Up _ _)) = Event (negateVector $ direction k)
pvChange (Event _) = NoEvent
pvChange NoEvent = NoEvent
