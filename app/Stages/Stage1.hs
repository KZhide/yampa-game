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
      ((vector2 100.0 80.0 & line' (vector2 (-20.0) 0.0) 5.0 |> line' (vector2 0.0 (-20.0)) 5.0) >>> arr fst)
      (aimShot 0.8 30.0)
      (arr snd >>> oArea)
  ] >>> arr Output
  where
    oArea = outOfArea (-100.0) 100.0 100.0 (-100.0)
