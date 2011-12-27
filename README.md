# Monono - Cute casual framework for OpenGL interactive graphics.

You may find yourself, at times, in the need of presenting your work in
a way that is friendly to others. It may be that you need to make some
interactive data visualization for the guys at market research,
an animated presentation to keep investors awed, or a simple game for
showing off your leet haxor skills to your loved ones during the holidays.

Monono is a tool for that kind of casual game developers who are also
haskell lovers. Monono knows that all you want is to draw a few images
on the screen, have them move when you click them or press your keyboard
arrows, animate them in some way and maybe have them explode when they collide.
It can and should be considered an option for haskellers who find
themselves going back to Flash, Processing or Javascript for building
this type of casual interactive visual applications.

That said, it can also be used as a playground to make the process of learning
haskell a bit more enjoyable.

Look at the examples to get an idea of what Monono is all about,
but just because it looks good, I'm adding the helloworld example here.

```haskell
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

```

What's pending? A lot! Since this version was developed very naively during a much
appreciated part-time break given to me by Max Cantor, my haskeller mentor
and startup partner.

* Implementation is too naive, sprites still pass the full state around when they could be passing deltas.
* Performance is not good, although not the main concern, should and could be better.
* Removing sprites, it's not supported yet.
* I want to have a way to query sprites as if they were an HTML DOM.
  So I can just query all the 'paddles' currently on the screen.
* Nameless children: Related to the above, if you can tag your sprites then you can add
  anonymous ones instead of having to give them a path.
* Propper event propagation and multiple handlers for the same event.
* Rendering Text.
