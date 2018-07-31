{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Shared

data Output = Output {
  pos :: Vec2,
  bulletSpawn :: Event [B.Bullet]
}

type Enemy = SFState ObjInput Output ()

runEnemy :: Enemy -> Vec2 -> SF ObjInput (Output, Event ())
runEnemy e v = runSFState e (Output v NoEvent)

enemy :: SFState ObjInput Vec2 ()
  -> SFState (ObjInput, Vec2) (Event [B.Bullet]) ()
  -> SFState (ObjInput, Vec2) () ()
  -> Enemy
enemy posSFState bSpawnSFState destroySFState =
  sfstate (
    \Output{pos} ->
      proc i -> do
        (p, moveEnd) <- runSFState posSFState pos -< i
        (bsEv, bsEnd) <- runSFState bSpawnSFState NoEvent -< (i, p)
        (_, dEv) <- runSFState destroySFState () -< (i, p)
        returnA -< (Output p bsEv, moveEnd `lMerge` dEv))

enemyMove posSFState = enemy posSFState (sfstate (const $ never &&& never)) (sfstate (const $ constant () &&& never))

draw :: Output -> Picture
draw o = (transV2 . pos) o $ Color blue $ Circle 8.0
