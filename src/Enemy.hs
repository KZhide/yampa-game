{-# LANGUAGE Arrows #-}

module Enemy where
import Defs
import Graphics.Gloss
import FRP.Yampa
import FRP.Yampa.Vector2
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Bullet as B
import YampaGlossInterface
import Control.Monad
import ObjInput

data Output = Output {
  state :: ObjState,
  bulletSpawn :: Event [B.Bullet]
}

type Enemy = SF ObjInput (Output, Event ObjState)

enemy :: SF ObjInput (ObjState, Event a) -> SF (ObjInput, ObjState) (Event [B.Bullet], Event b) -> SF (ObjInput, ObjState) (Event c) -> Enemy
enemy posSF bSpawnSF destroySF = proc i -> do
  (st, moveEnd) <- posSF -< i
  (bsEv, bsEnd) <- bSpawnSF -< (i, st)
  dEv <- destroySF -< (i, st)
  returnA -< (Output st bsEv, (void moveEnd `lMerge` void dEv) `tag` st)

enemyF ::
  (ObjState -> SF ObjInput (ObjState, Event ObjState)) ->
  SF (ObjInput, ObjState) (Event [B.Bullet], Event b) ->
  SF (ObjInput, ObjState) (Event c) ->
  ObjState -> Enemy
enemyF posSFf bSpawnSF destroySF st = enemy (posSFf st) bSpawnSF destroySF
enemyMoveF posSFf = enemyF posSFf (never &&& never) never

draw :: Output -> Picture
draw o = (transV2 . p . state) o $ Color blue $ Circle 8.0
