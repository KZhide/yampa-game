module Stages.Stage1 where

import qualified Stage as S
import qualified Enemy as E
import qualified Bullet as B
import Enemies.Boss1
import Bullets.NWay
import Shared
import Data.Function ((&))
import FRP.Yampa.Vector2
import FRP.Yampa
import Defs

stage1 :: S.Stage
stage1 = S.stage (after 0.2 [boss1])
