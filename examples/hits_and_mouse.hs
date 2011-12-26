{- |
  This example demonstrates how to use Collision detection and how to do drag and drop.
  The diamond follows your mouse everywhere and gets the color of the
  square it touches.
  You can mouse down on the blue square and drop it somewhere else.
  The diamond will stop following you when you move into the red square
  because the red square also captures mouse movements and is 'closer' than the root
  (it has a higher z-index)
  The diamond will also stop following you when you drag the blue disk
  but that's because you have a button down and that's dispatched as an 'onDrag'
  to the root sprite.
-}
module Main where

import Graphics.UI.Monono
import qualified Data.List as DL

main :: IO ()
main = runGame $ do
  window "Collisions and Mouse movement" 450 450
  withRoot $ do
    say "Touch the other objects with the diamond, drag and drop the blue disk"

    insert "red" $ do
      setCoords 50 250
      rectangle Polygon (red 1) 0 0 150 150
      onMouseMove $ do
        global <- getGlobalMousePos
        local <- getMousePos
        say $ "The global mouse position is "++ (show global)
        say $ "But with respect to this square's origin it is" ++ (show local)

    -- The blue disk can be dragged and dropped
    insert "blue" $ do
      setCoords 250 80
      disk FillStyle (blue 1) 0 0 0 80
      
      onMouseDown $ do
        -- We don't want to drag the blue disk from its center of gravity
        -- we rather drag it from the point were the mouse was originally pressed.
        (origx,origy) <- getMousePos
        onTick $ do
          -- This is the mouse position with respect to our parent (which is "/" btw)
          (x,y) <- with ".." $ getMousePos
          setCoords (x-origx) (y-origy)
      
      -- And stop dragging when we're done.
      onMouseUp $ clearOnTick

    -- This shows that Sprite nesting works intuitively even when you're just being silly.
    -- "d" is the diamond, notice that it is actually just a square, but since its
    -- parents are rotated it ends up appearing rotated at 45 degrees.
    insert "a" $ do
      setCoords 20 0
      setRotation 20
      insert "b" $ do
        setRotation 25
        insert "c" $ do
          insert "d" $ do
            setCoords 100 50
            
            -- Let's pre-apply our drawing function so we can just change the color
            -- yellow, red and blue are some default colors provided by Monono.
            let drawIt color = rectangle LineLoop (color 1) 0 0 90 90

            -- Starts yellow
            drawIt yellow

            onTick $ do
              hitting "/red" $ drawIt red
              hitting "/blue" $ drawIt blue

              -- We could also just get all the hits and do something with that.
              hits <- getHits
              case DL.length hits of
                0 -> drawIt yellow -- Back to yellow
                2 -> say $ "I don't know what to do, I'm hitting " ++ (show hits)
                _ -> return ()
            
    -- The mouse movement handlers are set on root instead of the diamond because
    -- the diamond only gets the onMouseMove event when the mouse is over it.
    -- If you set it on the diamond and move the mouse too fast, the mouse may escape.
    onMouseMove $ do
      with "/a/b/c" $ do
        (x,y) <- getMousePos
        with "d" $ setCoords x y

    -- OnMouseMove only triggers when the mouse is being moved with all its buttons Up
    -- otherwise the onDrag event is triggered. Drag means te gesture of pressing
    -- a mouse button and then releasing it. For the actual dragging of objects you
    -- may find yourself using onMouseMove and onTick a lot more.
    onDrag $ say $ "I don't do anything on drag, except for printing this message."

