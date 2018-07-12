module Stages.Stage1 where

import FRP.Yampa

import Stage
import qualified Enemy as E
import Enemies.AimShooter
import Shared

stage1 :: Stage
stage1 = repeatedly 2.0 [E.enemy (E.line (100.0, 80.0) (-100.0, 80.0) 20.0) (aimShot 0.8 30.0) oArea] >>> arr Output
  where
    oArea = outOfArea (-100.0) 100.0 100.0 (-100.0)
