{-#LANGUAGE Arrows#-}
{-#LANGUAGE NamedFieldPuns#-}
{-#LANGUAGE TupleSections#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE MultiParamTypeClasses#-}
module SFState
  ( SFState(runSFState)
  , sfstate
  , sfconc
  , noReturn
  , oneshot
  , delayCalc
  , sfPause
  )where

import FRP.Yampa
import Defs
import Control.Monad.State

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
