{-#LANGUAGE NamedFieldPuns#-}
module Enemies.Boss1 where

import qualified Bullet as B
import Enemy (Enemy, enemy)
import Bullets.NWay
import FRP.Yampa
import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace
import Defs
import ObjInput
import Shared
import Data.Function ((&))

boss1 :: Enemy
boss1 = enemy (returnO ObjState{p = vector2 0.0 240.0, v = vector2 0.0 (-50.0)}) (never &&& never) never
  |=>> entrance
  --normalAttack |=>>
  --ret |=>>
  --attack1 |=>>
  --ret |=>>
  --attack2 |=>>
  --ret |=>>
  --bye
  where
    entrance st = enemy (st & moveDuring 1.0) (never &&& never) never
--  enemy 
--    (ObjState{p = vector2 0.0 80.0, v = vector2 0.0 (-20.0)} &
--      (moveDuring 0.5 |=>> rot 60.0 |=>> moveDuring 0.5 |=>> rot 60.0 |=>> moveDuring 3.0))
--    (wait_ 0.3 |>> recur_ 2 (attack2 |>> wait_ 0.3))
--    never
    fast3way :: SF (ObjInput, ObjState) (Event [B.Bullet], Event ())
    fast3way = recur_ 4 (shotWait 0.3 (aimNWay 2 30.0 100.0)) |>> wait_ 0.2
    attack1 = recur_ 3 (shotWait 0.5 $ aimedAllWay 10 50.0) |<>| fast3way
    bomb :: Vec2 -> (ObjInput, ObjState) -> [B.Bullet]
    bomb v (_, ObjState{p=p}) = [B.bullet (move ObjState{v=v, p=p} >>> arr fst) (wait_ 0.6 |>> shotWait 0.3 (allWay 8 (vector2 0.0 (-40.0))) >>> arr fst) (after 0.6 ())]
    attack2 :: SF (ObjInput, ObjState) (Event [B.Bullet], Event ())
    attack2 = recur_ 10 (shotWait 1.3 $ circleShot 10 30.0 (vector2 10.0 (-30.0)))
    circleShot :: Int -> Float -> Vec2 -> (ObjInput, ObjState) -> [B.Bullet]
    circleShot n r v0 (_, ObjState{p, v}) =
      [
        let th = 2.0 * pi * (fromIntegral m / fromIntegral n) in
        B.simpleBullet ObjState{v = v0, p = p ^+^ vector2Polar r th}
        | m <- [0..n-1]
      ]
