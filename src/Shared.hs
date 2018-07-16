{-#LANGUAGE Arrows#-}
module Shared where

import FRP.Yampa
import FRP.Yampa.Vector2
import Defs

outOfArea :: Float -> Float -> Float -> Float -> SF Vec2 (Event ())
outOfArea l r t b = arr (f l r t b) >>> edge
  where
    f l r t b v = vector2X v < l || vector2X v > r || vector2Y v < b || vector2Y v > t

infixl 2 |=>>
(|=>>) :: (b -> SF a (b, Event ())) -> (b -> SF a (b, Event ())) -> b -> SF a (b, Event ())
(sfgen1 |=>> sfgen2) b =
  dSwitch (sfgen1 b >>> identity &&& arr (uncurry tagWith)) sfgen2

(|>>) :: SF a (b, Event ()) -> SF a (b, Event ()) -> SF a (b, Event ())
sf1 |>> sf2 =
  switch (sf1 >>> identity &&& arr (uncurry tagWith)) (const sf2)

move :: Vec2 -> Vec2 -> SF a (Vec2, Event ())
move v p = constant v >>> imIntegral p &&& constant NoEvent

moveDuring :: Time -> Vec2 -> Vec2 -> SF a (Vec2, Event ())
moveDuring t v p = move v p >>> second (time >>> arr (>t) >>> edge)

wait_ :: Time -> SF a (Event b, Event ())
wait_ t = constant NoEvent &&& after t ()

recur :: Int -> (b -> SF a (b, Event ())) -> (b -> SF a (b, Event ()))
recur 0 sf = \b -> (b, Event ()) --> constant (b, NoEvent)
recur n sf = sf |=>> recur (n-1) sf

recur_ :: Int -> SF a (b, Event ()) -> SF a (b, Event())
recur_ 1 sf = sf
recur_ n sf = sf |>> recur_ (n-1) sf

shotWait :: Time -> (a -> b) -> SF a (Event b, Event ())
shotWait t f = proc a -> do
  b <- arr f -< a
  ev <- now () -< a
  tev <- after t () -< a
  returnA -< (tag ev b, tev)
