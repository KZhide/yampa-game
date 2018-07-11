{-# LANGUAGE Arrows #-}
module Stage where

import FRP.Yampa

import qualified Enemy as E

type Input = ()
data Output = Output { eSpawn :: Event [E.Enemy] }
type Stage = SF Input Output

stage1 :: Stage
stage1 = repeatedly 5.0 [E.enemy (100.0, 80.0) (-30.0, 0.0) 4.1] >>> arr Output
