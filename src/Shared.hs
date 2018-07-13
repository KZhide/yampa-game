module Shared where

import FRP.Yampa
import Defs

outOfArea :: Float -> Float -> Float -> Float -> SF (a, Vec2) (Event ())
outOfArea l r t b = arr (f l r t b . snd) >>> edge
  where
    f l r t b (x, y) = x < l || x > r || y < b || y > t

infixl 2 |>
(|>) :: (b -> SF a (b, Event ())) -> (b -> SF a (b, Event ())) -> b -> SF a (b, Event ())
(sfgen1 |> sfgen2) b =
  dSwitch (sfgen1 b >>> identity &&& arr (uncurry tagWith)) sfgen2

line' :: Vec2 -> Time -> Vec2 -> SF a (Vec2, Event ())
line' v t p = (constant v >>> imIntegral p) &&& (time >>> arr (>t) >>> edge)


timeout :: Time -> SF a (Event ())
timeout t = after t ()
