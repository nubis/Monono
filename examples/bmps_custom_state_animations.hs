{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
  This example demonstrates how to use raster images instead of drawing primitives.
  It also shows how sprites can maintain their own custom state and how useful
  that can be for interactive animations.

  As a downside, it demonstrates how square bounding boxes can be a PITA, as you will see
  collisions when the corners of a bounding box touch something.
-}
module Main where

import Graphics.UI.Monono
import Control.Monad
import qualified Data.List as DL

main :: IO ()
main = runGame $ do
  window "Mouse and keyboard events, alpha, reusability" 700 700
  images [("sun.bmp", Just [(255,0,255)]), ("planet.bmp", Just [(255,0,255)] )]

  withRoot $ do
    -- For this example, having the center of the window be the center of gravity
    -- of the root sprite will be more intuitive. So keep in mind that 0,0 coordinates
    -- on children will actually be the center of the screen.
    setCoords 350 350

    insert "sun" $ do
      image 0 (-50) (-50) 100 100
      onTick $ do
        rotation (+0.1)
        s <- getScaling
        when (s > 1) $ scaling $ down 0.01
      onMouseDown $ scaling $ up 2

    insert "planet" $ celestial 200 1 (-4) $ do
      insert "moon" $ celestial 70 10 10 $ setScaling 0.1
      insert "moon2" $ celestial 50 (-5) 2 $ setScaling 0.3

    insert "planet2" $ celestial 100 0 0 $ do
      insert "moon2" $ celestial 50 0 2 $ setScaling 0.2

    insert "planet3" $ celestial 300 (-3) (-4) $ do
      insert "moon" $ celestial 70 10 (-3) $ setScaling 0.1
      insert "moon2" $ celestial 50 (-3) 1 $ setScaling 0.3
      insert "moon3" $ celestial 100 4 10 $ setScaling 0.6

-- A celestial is a circle of a given size that orbits at
-- a distance and at a certain speed, rotating at rotSpeed on its axis.
-- Clicking on a planet increases it's orbit speed.
-- For each celestial we have 2 sprites, the parent does orbiting and the 
-- child does rotation and has more children
celestial :: GLfloat -> GLfloat -> GLfloat ->  Sprite () -> Sprite ()
celestial distance orbitSpeed rotSpeed more = do
  setExtra $ orbitSpeed
  onTick $ do
    s <- getExtra orbitSpeed -- Get the extra data that was stored or default 'speed'
    rotation $ up s

  -- This is the actual celestial body
  insert "actualPlanet" $ do
    setCoords distance 0
    let normal = image 1 (-30) (-30) 60 60
    normal
    onTick $ do
      rotation $ up rotSpeed
      hits <- getHits
      if "/sun" `DL.elem` hits
        then disk FillStyle (red 1) 0 0 0 30
        else normal
    onMouseDown $ with ".." $ extra $ up (1::GLfloat)
    more

