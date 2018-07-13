module Stages.Stage1 where

import FRP.Yampa

import Stage
import qualified Enemy as E
import Enemies.AimShooter
import Shared
import Data.Function ((&))
import FRP.Yampa.Vector2

stage1 :: Stage
stage1 = repeatedly 2.0 [
    E.enemy 
      ((vector2 100.0 80.0 & moveDuring 3.0 (vector2 (-60.0) 0.0) |=>> move (vector2 0.0 (-20.0))) >>> arr fst)
      (aimShot 0.8 30.0)
      (arr snd >>> oArea)
  ] >>> arr Output
  where
    oArea = outOfArea (-100.0) 100.0 100.0 (-100.0)
