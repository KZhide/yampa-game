{-# LANGUAGE Arrows #-}
module Stage (Stage, stage, eSpawn)where

import FRP.Yampa

import qualified Enemy as E
import Defs

type Input = ()
data Output = Output { eSpawn :: Event [(E.Enemy, Vec2)] }
type Stage = SF Input Output

stage :: SF () (Event [(E.Enemy, Vec2)]) -> Stage
stage sf = sf >>> arr Output
