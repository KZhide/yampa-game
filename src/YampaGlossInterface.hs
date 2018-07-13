module YampaGlossInterface where

import FRP.Yampa.Vector2
import Graphics.Gloss.Data.Picture

transV2 :: Vector2 Float -> Picture -> Picture
transV2 v = translate (vector2X v) (vector2Y v)
