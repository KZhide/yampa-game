module Enemies.Boss1 where

import qualified Bullet as B
import qualified Enemy as E
import Bullets.NWay
import FRP.Yampa
import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace
import Defs
import Shared
import Data.Function ((&))

boss1 :: E.Enemy
boss1 =
  E.enemy 
    ((vector2 0.0 80.0 & move zeroVector) >>> arr fst)
    (wait_ 0.3 |>> recur_ 2 (attack1 |>> wait_ 0.3) >>> arr fst)
    (arr snd >>> oArea)
  where
    oArea = outOfArea (-100.0) 100.0 100.0 (-100.0)
    fast3way :: SF (E.Input, Vec2) (Event [B.Bullet], Event ())
    fast3way = recur_ 4 (shotWait 0.3 (aimNWay 2 30.0 100.0)) |>> wait_ 0.2
    attack1 = recur_ 3 (shotWait 0.5 $ aimedAllWay 10 50.0) |<>| fast3way
