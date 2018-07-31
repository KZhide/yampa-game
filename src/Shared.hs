{-#LANGUAGE Arrows#-}
{-#LANGUAGE NamedFieldPuns#-}
{-#LANGUAGE TupleSections#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE MultiParamTypeClasses#-}
module Shared where

import FRP.Yampa
import FRP.Yampa.Vector2
import Defs
import Data.Semigroup
import ObjInput
import Control.Monad.State

deg2rad :: Float -> Float
deg2rad deg = deg / 360.0 * 2.0 * pi

outOfArea :: Vec2 -> Bool
outOfArea p = let (x, y) = vector2XY p in
  x < -320.0 || x > 320.0 || y < -240.0 || y > 240.0

newtype SFState i s a = SFState {runSFState :: s -> SF i (s, Event a)}
sfstate = SFState

(|==>>) :: SFState i s a -> (a -> SFState i s b) -> SFState i s b
sfst |==>> mf = SFState (runSFState sfst |==> (runSFState . mf))
  where
  (|==>) :: (b -> SF a (b, Event c)) -> (c -> b -> SF a (b, Event d)) -> b -> SF a (b, Event d)
  (b2sf |==> cd2sf) b =
    let sf =
          proc a -> do
            (b', evc) <- b2sf b -< a
            returnA -< ((b', NoEvent), evc `attach` b')
    in
    switch sf (uncurry cd2sf)

instance Functor (SFState i s) where
  fmap f SFState {runSFState} = SFState ((>>> second (arr (fmap f))) . runSFState)

sfconc :: SF (s, Event a) (s, Event b) -> SFState i s a -> SFState i s b
sfconc sf sfst = sfstate ((>>> sf) . runSFState sfst)

instance Applicative (SFState i s) where
  pure a = SFState (\s -> constant s &&& now a)
  sfst1 <*> sfst2 = sfst1 |==>> (\f -> fmap f sfst2)

instance Monad (SFState i s) where
  return a = SFState {runSFState = \s -> constant s &&& now a }
  (>>=) = (|==>>)

noReturn :: SFState i s a
noReturn = sfstate (\s -> constant s &&& never)

instance MonadState s (SFState i s) where
  get = SFState (\s -> constant s &&& now s)
  put s = SFState (\_ -> constant s &&& now ())

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

oneshot :: (i -> s) -> SFState i (Event s) ()
oneshot f = sfstate $
  \_ ->
    proc i -> do
      s <- arr (uncurry tag) <<< now () &&& arr f -< i
      ev <- now () -< ()
      returnA -< (s, ev)

delayCalc :: Time -> a -> SFState i (Event e) a
delayCalc t a = sfstate (\s -> (s, NoEvent) --> (never &&& after t a))

sfPause :: Time -> a -> SFState i s a
sfPause t a = sfstate (\s -> constant s &&& after t a)
