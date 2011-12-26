{- |
  It draws a yellow box that you can drag and drop with your mouse,
  enlarge using the Up/Down arrow keys and rotate it holding down the r key.
  The semi-transparent red circle moves when the box touches it.
-}
module Main where

import Graphics.UI.Monono
import Control.Monad.Trans.State

main :: IO ()
main = runGame $ do
  window "Hello World" 450 450
  withRoot $ do
    insert "square" $ do
      let half = 40
      let drawIt color = rectangle Polygon (color 1) (-half) (-half) (half*2) (half*2)
      drawIt yellow
      setCoords 100 300
      
      onMouseDown $ do
        (origx,origy) <- getMousePos
        onTick $ do
          (x,y) <- with ".." $ getMousePos
          setCoords (x-origx) (y-origy)
      onMouseUp $ clearOnTick

      onKeyDown $ do
        hitting "/disk" $ say "hitting it"
        key (SpecialKey KeyUp) $ scaling $ up 0.1
        key (SpecialKey KeyDown) $ scaling $ down 0.1

      onKeyHold $ isDown_ 'r' $ rotation $ up 20

    insert "disk" $ do
      setCoords 300 300
      disk FillStyle (red 0.5) 0 0 0 80
      onTick $ hitting "../square" $ do
        speed <- getExtra (10::GLfloat)
        x <- getXCoord
        setXCoord $ x+speed
        when (x > 400) $ setExtra $ (abs speed) * (-1)
        when (x < 300) $ setExtra $ abs speed

