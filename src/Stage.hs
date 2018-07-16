{-# LANGUAGE Arrows #-}
module Stage (Stage, stage, eSpawn)where

import FRP.Yampa

import qualified Enemy as E

type Input = ()
data Output = Output { eSpawn :: Event [E.Enemy] }
type Stage = SF Input Output

stage :: (SF () (Event [E.Enemy])) -> Stage
stage sf = sf >>> arr Output
