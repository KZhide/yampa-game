module Shared where

import FRP.Yampa
import Defs

outOfArea :: Float -> Float -> Float -> Float -> SF (a, Vec2) (Event ())
outOfArea l r t b = arr (f l r t b . snd) >>> edge
  where
    f l r t b (x, y) = x < l || x > r || y < b || y > t


timeout :: Time -> SF a (Event ())
timeout t = after t ()
