{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
  This example is Pong, therefore I consider that no further explainations are needed.
  There are some other examples in this directory which are not pong, and wich may
  or may not have comments satisfying your commenting needs.
  That said, I may comment some things here and there.
  ---
    Edit: don't scroll down there it's crazy.
  ---
    Edit from a moribund guy in grey outfit: run you fool!
-}
module Main where

import Graphics.UI.Monono
import Control.Monad
import qualified Data.List as DL
import Data.Typeable
import Random (randomRIO)

data BallState = BallState {speedx :: GLfloat, speedy :: GLfloat, deaths :: GLfloat }
  deriving Typeable

wWidth :: GLfloat
wWidth = 700
wHeight :: GLfloat
wHeight = 700

main :: IO ()
main = runGame $ do
  window "Pong man. Pong." (realToFrac wWidth) (realToFrac wHeight)
  images [("sun.bmp", Just [(255,0,255)]), ("planet.bmp", Just [(255,0,255)] )]

  withRoot $ do
    insert "paddle" $ do
      let w = 200
      setCoords ((wWidth - w) / 2) 20
      rectangle Polygon (magenta 1) 0 0 w 40
      onKeyHold $ do
        (x,y) <- getCoords
        isDown (SpecialKey KeyLeft) $ when (x > 0) $ xCoord $ down 10
        isDown (SpecialKey KeyRight) $ when (x < (wWidth - w)) $ xCoord $ up 10

    insert "ball" $ makeBall

randomSpeed :: Sprite GLfloat
randomSpeed = liftIO $ do
  r <- randomRIO (5,10::Int)
  return $ fromIntegral $ if (r `mod` 2) == 0 then r else r * (-1)
  
-- A ball bounces off the paddle, bounces off the walls,
-- keeps count of how many times it fell off the screen
-- and when that happens, it goes back to the middel and replicates itself.
makeBall :: Sprite ()
makeBall = do
  setCoords (wWidth/2) (wHeight/2)
  let r = 15
  ri <- liftIO $ randomRIO (0,1::Int)
  image ri (-r) (-r) (r*2) (r*2)

  rx <- randomSpeed
  ry <- randomSpeed
  onTick $ do
    (x,y) <- getCoords
    BallState xs ys p <- getExtra $ BallState rx ry 0
    let nextx = x + xs
        nexty = y + ys

    setCoords nextx nexty

    when (nextx < r) $ do
      setXCoord r
      extra $ \ e -> e { speedx = xs * (-1) }

    when (nextx > (wWidth - r)) $ do
      setXCoord $ wWidth - r
      extra $ \ e -> e { speedx = xs * (-1) }

    when (nexty > (wHeight - r)) $ do
      setYCoord $ wHeight - r
      extra $ \ e -> e { speedy = ys * (-1) }

    hitting "/paddle" $ do 
      extra $ \e -> e { speedy = (abs $ speedy e) }

    when (nexty < r) $ do
      setCoords (wWidth/2) (wHeight/2)
      rx' <- randomSpeed
      ry' <- randomSpeed
      extra $ \ e -> BallState rx' ry' (p+1)
      ballPath <- getSprite spritePath
      say $ "Ball "++ballPath++" died " ++ (show $ p + 1) ++ " times"

      with "/" $ do
        balls <- getExtra (0::Int)
        insert (show balls) $ makeBall
        setExtra $ balls + 1
