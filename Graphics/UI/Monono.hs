{- |
Monono is a framework for casual game development that aims to be simple, expressive, 
intutitve and hacker friendly.

It's currently focused on 2D games but support for 2.5D and 3D is planned.

Monono is built using OpenGL and GLUT and exposes features of those libraries directly.

To put it simply, a Monono game is a tree of Sprites.

A Sprite is an object on the screen that can be moved, scaled and rotated. Transforming
a Sprite affect its children, so if you rotate a sprite all its children and gran
children will rotate as if they all were part of a single image.
Sprites can be added and modified freely at runtime.

Sprites respond to events triggered by the user like clicks and keystrokes, or by the
game itself like when a new frame is rendered or when two sprites collide. 
Event handlers can be bound, re-bound, and unbound at any time.
All event handlers can perform IO by using liftIO.

Sprites also hold arbitrary extra state which enables us to write games in a
semi-object oriented way. We can have a Spaceship Sprite that keeps track of 
the fuel it has left.

A Sprite may be represented by a graphic on the screen, but it may also be used
for listening to events or as a mere wrapper to its children.

A sprite's graphic representation is a function that runs OpenGL drawing comands,
so it can be any 2D or 3D composition of primitives, with changing colors and textures.
Shortcut functions are provided for drawing the most common 2D shapes and images.

To learn how to use Monono I reccomend looking at the examples, but since it's so
short, here's the helloworld example:

It draws a yellow box that you can drag and drop with your mouse,
enlarge using the Up/Down arrow keys and rotate it holding down the r key.
The semi-transparent red circle moves when the box touches it.

>  module Main where
>
>  import Graphics.UI.Monono
>  import Control.Monad.Trans.State
>
>  main :: IO ()
>  main = runGame $ do
>    window "Hello World" 450 450
>    withRoot $ do
>      insert "square" $ do
>        let half = 40
>        let drawIt color = rectangle Polygon (color 1) (-half) (-half) (half*2) (half*2)
>        drawIt yellow
>        setCoords 100 300
>        
>        onMouseDown $ do
>          (origx,origy) <- getMousePos
>          onTick $ do
>            (x,y) <- with ".." $ getMousePos
>            setCoords (x-origx) (y-origy)
>        onMouseUp $ clearOnTick
>
>        onKeyDown $ do
>          hitting "/disk" $ say "hitting it"
>          key (SpecialKey KeyUp) $ scaling $ up 0.1
>          key (SpecialKey KeyDown) $ scaling $ down 0.1
>
>        onKeyHold $ isDown_ 'r' $ rotation $ up 20
>
>      insert "disk" $ do
>        setCoords 300 300
>        disk FillStyle (red 0.5) 0 0 0 80
>        onTick $ hitting "../square" $ do
>          speed <- getExtra (10::GLfloat)
>          x <- getXCoord
>          setXCoord $ x+speed
>          when (x > 400) $ setExtra $ (abs speed) * (-1)
>          when (x < 300) $ setExtra $ abs speed

-}

module Graphics.UI.Monono
  ( module Graphics.UI.Monono.Game
  , module Graphics.UI.Monono.Sprite
  , module Graphics.UI.Monono.Types
  , module Graphics.UI.Monono.Drawing
  , module Graphics.UI.Monono.Input
  , module Graphics.UI.Monono.Collisions
  , module Graphics.UI.GLUT
  , liftIO
  , when
  )
where

import Graphics.UI.Monono.Game
import Graphics.UI.Monono.Sprite
import Graphics.UI.Monono.Types
import Graphics.UI.Monono.Drawing
import Graphics.UI.Monono.Input
import Graphics.UI.Monono.Collisions
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.GLUT hiding (get)
import Control.Monad (when)

