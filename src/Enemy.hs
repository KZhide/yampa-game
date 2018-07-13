{-# LANGUAGE Arrows #-}

module Enemy where
import Defs
import Graphics.Gloss
import FRP.Yampa
import FRP.Yampa.Vector2
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Bullet as B
import YampaGlossInterface

data Input = Input {
  pPos :: Vec2
}

data Output = Output {
  pos :: Vec2,
  bulletSpawn :: Event [B.Bullet],
  destroy :: Event ()
}

type Enemy = SF Input Output

enemy :: SF Input Vec2 -> SF (Input, Vec2) (Event [B.Bullet]) -> SF (Input, Vec2) (Event ()) -> Enemy
enemy posSF bSpawnSF destroySF = proc i -> do
  p <- posSF -< i
  bsEv <- bSpawnSF -< (i, p)
  dEv <- destroySF -< (i, p)
  returnA -< Output p bsEv dEv

draw :: Output -> Picture
draw o = (transV2 . pos) o $ Color blue $ Circle 8.0
