{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
  This example is about mouse and keyboard events.
  It shows: 
    * Some of the functions to retrieve which keys are being pressed or held.
    * All the mouse events and usage of z-index.
    * A small idiom for parametrizing sprite creation.
-}
module Main where

import Graphics.UI.Monono

main :: IO ()
main = runGame $ do
  window "Mouse and keyboard events, alpha, reusability" 450 450
  background $ white 1
  withRoot $ do

    -- onKeyDown triggers only when the key is pressed for the first time
    onKeyDown $ do
      -- key_ and key let you specify which keys to respond to
      key_ 'a' $ say "Pressed 'a'"

      -- if you need more granularity, you can query what the last key was.
      lastkey <- getLastKey
      say $ "Your last pressed key was: "++ (show lastkey)

      -- Or a sligtly more expressive version, telling what you're looking for.
      isG <- keyIsLast (Char 'g')
      if isG
        then say $ "Your last pressed key was g"
        else say $ "Your last pressed key was not g"

      -- While not triggering this event, you can check if any other key
      -- is up or down at the moment of this key press.
      isDown_ 'g' $ say "And g was also down"

      -- You can check if any of several keys were down too.
      anyDown_ "gal" $ say "if you hold down 'gal' you'll get a surprise"

      -- Modifiers are also accessible 
      withCtrl $ say "You're in control"
      withShift $ say "You are yelling now"

      say "" -- Enjoy the silence.
      
    -- onKeyHold is triggered once every tick while any keys
    -- are being held down.
    -- Notice how you can check for several keys being down at once.
    onKeyHold $ allDown_ "gal" $ say "Galileo figarooooo!!"
    
    -- These are all clones, showing how easy is to parametrize Sprites
    -- to avoid repeating yourself.
    insert "red" $ squareClone "red" 50 50 red
    insert "green" $ squareClone "green" 50 200 green
    
    -- Look, the yellow one has a specific z-index.
    -- Sprites are usually added at the top (closest to you), but this one
    -- will be shown on top of the blue square that we draw below.
    insert "yellow" $ do
      setZ 0.1
      squareClone "yellow" 200 50 yellow

    -- This clone also does an onKeyDown, checking for special keys in order
    -- to move the blue square or the whole thing up and down.
    -- Yes, it may have been more convenient to bind KeyTop and KeyDown on 
    -- the root sprite, but just wanted to show you how easy you can tweak other things.
    insert "blue" $ do
      squareClone "blue" 200 200 blue
      onKeyDown $ do
        key (SpecialKey KeyRight) $ xCoord $ up 10
        key (SpecialKey KeyLeft) $ xCoord $ down 10
        key (SpecialKey KeyUp) $ do
          with "/" $ yCoord $ up 10
          with "/red" $ yCoord $ up 10
        key (SpecialKey KeyDown) $ do
          with ".." $ yCoord $ down 10
          with "/red" $ yCoord $ down 10

-- This shows one idiom for re-using code and parametrizing sprites
squareClone name x y color = do
  setCoords x y
  rectangle Polygon (color 0.4) 0 0 200 200
  onMouseDown $ say $ "OnMouseDown: " ++ name
  onMouseUp $ say $ "OnMouseUp: " ++ name
  onClick $ say $ "OnClick:  " ++ name
  onMouseMove $ say $ "OnMouseMove: " ++ name
  onMouseEnter $ say $ "OnMouseEnter: " ++ name
  onMouseLeave $ say $ "OnMouseLeave: " ++ name
  onDropIn $ say $ "OnDropIn: " ++ name
  onDropOut $ say $ "OnDropOut: " ++ name
  onDrag $ say $ "OnDrag: " ++ name
  onDragIn $ say $ "OnDragIn: " ++ name
  onDragOut $ say $ "OnDragOut: " ++ name
