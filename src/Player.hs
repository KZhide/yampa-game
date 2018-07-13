{-# LANGUAGE Arrows #-}
--module Player (PlayerState, player, playerVelocity, drawEntity) where
module Player (player, Input(Input), Output(..), draw) where

import FRP.Yampa
import FRP.Yampa.VectorSpace
import FRP.Yampa.Vector2
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import Defs
import YampaGlossInterface
import qualified Bullet as B

data Input = Input (Event G.Event) deriving (Eq, Show)
data Output = Output {pos :: Vec2, shot :: Event [B.Bullet]}

type Player = SF Input Output

keyOnOff :: G.Key -> SF (Event G.Event) (Event Bool)
keyOnOff = arr . keyEvent
  where
    keyEvent k (Event (G.EventKey k' s _ _)) | k == k' = Event (s == G.Down)
                                             | otherwise = NoEvent
    keyEvent k _ = NoEvent

bulletSpawn :: SF (Event G.Event) (Event [B.Bullet])
bulletSpawn = proc e -> do
  eventShotKey <- keyOnOff (G.SpecialKey G.KeySpace) -< e
  newsf <- arr (fmap shotSF) -< eventShotKey
  bs <- drSwitch never -< (e, newsf)
  returnA -< bs
  where
    shotSF :: Bool -> SF a (Event [B.Bullet])
    shotSF True = repeatedly 0.1 [B.simpleBullet zeroVector (vector2 0.0 10.0)]
    shotSF False = never

player :: Vec2 -> Player
player p0 = proc (Input e) -> do
  sp <- bulletSpawn -< e
  v <- playerVelocity -< e
  p <- integral -< v
  --bs <- arr (uncurry tag) <<< repeatedly 1.0 () &&& arr ((:[]). uncurry B.bullet) -< (p, v)
  returnA -< Output p sp

playerVelocity :: SF (Event G.Event) Vec2
playerVelocity = (constant zeroVector &&& arr pvChange) >>> impulseIntegral

draw :: Output -> Picture
draw (Output v _) = transV2 v $ color red (Circle 12.0)

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
