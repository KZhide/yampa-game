{-#LANGUAGE Arrows#-}
{-#LANGUAGE NamedFieldPuns#-}
module Shared where

import FRP.Yampa
import FRP.Yampa.Vector2
import Defs
import Data.Semigroup
import ObjInput

deg2rad :: Float -> Float
deg2rad deg = deg / 360.0 * 2.0 * pi

outOfArea :: ObjState -> Bool
outOfArea ObjState{p} = let (x, y) = vector2XY p in
  x < -320.0 || x > 320.0 || y < -240.0 || y > 240.0

infixl 2 |=>>
(|=>>) :: (b -> SF a (b, Event ())) -> (b -> SF a (b, Event ())) -> b -> SF a (b, Event ())
(sfgen1 |=>> sfgen2) b =
  switch (sfgen1 b >>> identity &&& arr (uncurry tagWith)) sfgen2

(|>>) :: SF a (b, Event ()) -> SF a (b, Event ()) -> SF a (b, Event ())
sf1 |>> sf2 =
  switch (sf1 >>> identity &&& arr (uncurry tagWith)) (const sf2)

(|<>|) :: Semigroup b => SF a (Event b, Event ()) -> SF a (Event b, Event ()) -> SF a (Event b, Event ())
sf1 |<>| sf2 = proc a -> do
  (evcolb1, ev1) <- sf1 -< a
  (evcolb2, ev2) <- sf2 -< a
  b1 <- hold False <<< arr (fmap (const True)) -< ev1
  b2 <- hold False <<< arr (fmap (const True)) -< ev2
  ev <- edge <<<  arr (uncurry (&&)) -< (b1, b2)
  returnA -< (mergeBy (<>) evcolb1 evcolb2, ev)

move :: ObjState -> SF a (ObjState, Event ())
move ObjState{v, p} = constant v >>> (imIntegral p >>> arr (\p -> ObjState {v=v, p=p})) &&& constant NoEvent

rot :: Float -> ObjState -> SF a (ObjState, Event ())
rot degree ObjState{p, v} = constant ObjState{p, v = vector2Rotate (deg2rad degree) v} &&& now ()

setV :: Vec2 -> ObjState -> SF a (ObjState, Event ())
setV v' st = constant st{v = v'} &&& now ()

moveDuring :: Time -> ObjState -> SF a (ObjState, Event ())
moveDuring t st@ObjState{v, p} = move st >>> second (time >>> arr (>t) >>> edge)

moveWhile :: (ObjState -> Bool) -> ObjState -> SF a (ObjState, Event ())
moveWhile f st = move st >>> arr fst >>> identity &&& (arr f >>> edge)

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
