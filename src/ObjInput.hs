module ObjInput where

import Defs

newtype PlayerPos = PlayerPos Vec2
data ObjInput = ObjInput {pPos :: PlayerPos}
data ObjState = ObjState {p :: Vec2, v :: Vec2}
