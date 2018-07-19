{-# LANGUAGE Arrows #-}

module Enemy where
import Defs
import Graphics.Gloss
import FRP.Yampa
import FRP.Yampa.Vector2
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Bullet as B
import YampaGlossInterface
import ObjInput

data Output = Output {
  state :: ObjState,
  bulletSpawn :: Event [B.Bullet],
  destroy :: Event ()
}

type Enemy = SF ObjInput Output

enemy :: SF ObjInput ObjState -> SF (ObjInput, ObjState) (Event [B.Bullet]) -> SF (ObjInput, ObjState) (Event ()) -> Enemy
enemy posSF bSpawnSF destroySF = proc i -> do
  st <- posSF -< i
  bsEv <- bSpawnSF -< (i, st)
  dEv <- destroySF -< (i, st)
  returnA -< Output st bsEv dEv

draw :: Output -> Picture
draw o = (transV2 . p . state) o $ Color blue $ Circle 8.0
