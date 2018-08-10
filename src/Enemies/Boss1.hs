{-#LANGUAGE NamedFieldPuns#-}
module Enemies.Boss1 where

import qualified Bullet as B
import Enemy (Enemy, enemy, enemyMove)
import Bullets.NWay
import FRP.Yampa
import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace
import Defs
import ObjInput
import Shared
import Data.Function ((&))
import Control.Monad
import SFState

boss1 :: Enemy
boss1 = do
  ret
  enemy (move zeroVector) fast3way (sfPause 1.0 ())
  enemy (void $ moveTo 3.0 (vector2 100.0 0.0)) attack1 (sfPause 3.0 ())
  ret
  enemy (move zeroVector) fast3way (sfPause 1.0 ())
  enemy (move zeroVector) attack2 noReturn
  --normalAttack |=>>
  --ret |=>>
  --attack1 |=>>
  --ret |=>>
  --attack2 |=>>
  --ret |=>>
  --bye
  where
    ret = enemyMove (void $ moveTo 0.5 (vector2 0.0 120.0))
--  enemy 
--    (ObjState{p = vector2 0.0 80.0, v = vector2 0.0 (-20.0)} &
--      (moveDuring 0.5 |=>> rot 60.0 |=>> moveDuring 0.5 |=>> rot 60.0 |=>> moveDuring 3.0))
--    (wait_ 0.3 |>> recur_ 2 (attack2 |>> wait_ 0.3))
--    never
    fast3way :: SFState (ObjInput, Vec2) (Event [B.Bullet]) ()
    fast3way = do
      oneshot (aimNWay 2 30.0 100.0) >>= delayCalc 0.3
      oneshot (aimNWay 2 30.0 100.0) >>= delayCalc 0.3
      oneshot (aimNWay 2 30.0 100.0) >>= delayCalc 0.3
    attack1 = do
      oneshot (aimedAllWay 10 50.0) >>= delayCalc 0.5
      oneshot (aimedAllWay 10 50.0) >>= delayCalc 0.5
      oneshot (aimedAllWay 10 50.0) >>= delayCalc 0.5
    --bomb :: Vec2 -> (ObjInput, ObjState) -> [B.Bullet]
    --bomb v (_, ObjState{p=p}) = [B.bullet (move ObjState{v=v, p=p} >>> arr fst) (wait_ 0.6 |>> shotWait 0.3 (allWay 8 (vector2 0.0 (-40.0))) >>> arr fst) (after 0.6 ())]
    attack2 :: SFState (ObjInput, Vec2) (Event [B.Bullet]) ()
    attack2 = do
      oneshot (circleShot 10 30.0 (vector2 10.0 (-30.0))) >>= delayCalc 1.5
    circleShot :: Int -> Float -> Vec2 -> (ObjInput, Vec2) -> [B.Bullet]
    circleShot n r v0 (_, p) =
      [
        let th = 2.0 * pi * (fromIntegral m / fromIntegral n) in
        B.simpleBullet v0 (p ^+^ vector2Polar r th)
        | m <- [0..n-1]
      ]
