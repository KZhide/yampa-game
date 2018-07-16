module Stages.Stage1 where

import FRP.Yampa

import qualified Stage as S
import qualified Enemy as E
import qualified Bullet as B
import Enemies.AimShooter
import Bullets.NWay
import Shared
import Data.Function ((&))
import FRP.Yampa.Vector2
import Defs

stage1 :: S.Stage
stage1 = S.stage (Event [
    E.enemy 
      ((vector2 0.0 80.0 & move zeroVector) >>> arr fst)
--      ((repeatedly 0.3 () &&& arr (aimNWay 2 45.0 100.0)) >>> arr (uncurry tag))
      (recur_ 2 fast3way |>> recur_ 3 (shotWait 0.5 $ aimedAllWay 10 50.0)>>> arr fst)
      (arr snd >>> oArea)
  ] --> constant NoEvent)
  where
    oArea = outOfArea (-100.0) 100.0 100.0 (-100.0)
    fast3way :: SF (E.Input, Vec2) (Event [B.Bullet], Event ())
    fast3way = recur_ 20 (shotWait 0.1 (aimNWay 2 30.0 100.0)) |>> wait_ 0.2
