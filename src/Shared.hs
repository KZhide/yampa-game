{-#LANGUAGE Arrows#-}
{-#LANGUAGE NamedFieldPuns#-}
{-#LANGUAGE TupleSections#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE MultiParamTypeClasses#-}
module Shared where

import FRP.Yampa
import FRP.Yampa.Vector2
import Defs
import SFState
import Data.Semigroup
import ObjInput
import Control.Monad.State

deg2rad :: Float -> Float
deg2rad deg = deg / 360.0 * 2.0 * pi

outOfArea :: Vec2 -> Bool
outOfArea p = let (x, y) = vector2XY p in
  x < -320.0 || x > 320.0 || y < -240.0 || y > 240.0

(|<>|) :: Semigroup b => SF a (Event b, Event ()) -> SF a (Event b, Event ()) -> SF a (Event b, Event ())
sf1 |<>| sf2 = proc a -> do
  (evcolb1, ev1) <- sf1 -< a
  (evcolb2, ev2) <- sf2 -< a
  b1 <- hold False <<< arr (fmap (const True)) -< ev1
  b2 <- hold False <<< arr (fmap (const True)) -< ev2
  ev <- edge <<<  arr (uncurry (&&)) -< (b1, b2)
  returnA -< (mergeBy (<>) evcolb1 evcolb2, ev)

infiniteIntegral :: VectorSpace s a => SF i s -> SFState i s ()
infiniteIntegral sf = sfstate (\s -> sf >>> imIntegral s &&& never)

finiteIntegralWithDerivative :: VectorSpace s a => Time -> SF i s -> SFState i s s
finiteIntegralWithDerivative t sf =
  sfstate (\s ->
    proc i -> do
      dx <- sf -< i
      x  <- imIntegral s -< dx
      ev <- after t () -< i
      returnA -< (x, ev `tag` dx)
  )

rot :: Float -> Vec2 -> Vec2
rot degree = vector2Rotate (deg2rad degree)

move :: Vec2 -> SFState a Vec2 ()
move v = infiniteIntegral (constant v)

moveDuring :: Time -> Vec2 -> SFState a Vec2 Vec2
moveDuring t v = finiteIntegralWithDerivative t (constant v)

moveTo :: Time -> Vec2 -> SFState i Vec2 Vec2
moveTo t tgt = do
  src <- get
  moveDuring t ((tgt ^-^ src) ^/ realToFrac t)

