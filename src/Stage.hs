{-# LANGUAGE Arrows #-}
module Stage where

import FRP.Yampa

import qualified Enemy as E

type Input = ()
data Output = Output { eSpawn :: Event [E.Enemy] }
type Stage = SF Input Output
