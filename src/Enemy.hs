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
  bulletSpawn :: Event [B.Bullet]
}

type Enemy = SF ObjInput (Output, Event ())

enemy :: SF ObjInput (ObjState, Event ()) -> SF (ObjInput, ObjState) (Event [B.Bullet], Event ()) -> SF (ObjInput, ObjState) (Event ()) -> Enemy
enemy posSF bSpawnSF destroySF = proc i -> do
  (st, moveEnd) <- posSF -< i
  (bsEv, bsEnd) <- bSpawnSF -< (i, st)
  dEv <- destroySF -< (i, st)
  returnA -< (Output st bsEv, lMerge moveEnd dEv)

draw :: Output -> Picture
draw o = (transV2 . p . state) o $ Color blue $ Circle 8.0
