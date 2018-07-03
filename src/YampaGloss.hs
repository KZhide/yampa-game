module YampaGloss where

import FRP.Yampa
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import Data.IORef
import Control.Monad

playYampa :: Display -> Color -> Int ->
  world -> SF (Event G.Event) world -> (world -> Picture) -> IO ()

playYampa display color frequency initialWorld sf drawing = do
  wRef <- newIORef initialWorld
  handle <- reactInit
    (return NoEvent)
    (\_ changed world' -> do
      _ <- when changed $ wRef `writeIORef` world'
      return False)
    sf
  _ <- react handle (infts, Just NoEvent)
  G.playIO
    display
    color
    frequency
    initialWorld
    (return . drawing)
    (\e w -> react handle (0.0, Just (Event e)) >> readIORef wRef)
    (\delta w -> react handle (realToFrac delta, Just NoEvent) >> readIORef wRef)

  where
    infts = 0.01 / fromIntegral frequency
