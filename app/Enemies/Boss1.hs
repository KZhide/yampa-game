module Enemies.Boss1 where

import qualified Bullet as B
import qualified Enemy as E
import FRP.Yampa
import Defs
import Shared

attack1 :: SF (E.Input, Vec2) (Event [B.Bullet], Enevt ())
attack2 :: SF (E.Input, Vec2) (Event [B.Bullet], Enevt ())
attack3 :: SF (E.Input, Vec2) (Event [B.Bullet], Enevt ())
attack4 :: SF (E.Input, Vec2) (Event [B.Bullet], Enevt ())

a = attack1 |> attack2 |> randomChoice [attack3, attack4]
