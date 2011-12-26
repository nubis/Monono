{-# LANGUAGE OverloadedStrings #-}
{-| 
A 'Sprite''s visual representation is created through the 'draw' function, and other
auxiliary functions defined in this module.

The function you use for drawing receives a 'GameData' and executes
OpenGL draw OpenGL drawing procedures /in the IO monad/.

The /origin/ or /center of rotation/ of your sprite is at coordinates (0,0),
you can draw on negative x/y coordinates in order to change it.

For example, this is a sprite whose drawing is a 10x10 rectangle.
It's center of rotation will be the square's bottom left corner.

@
'insert' \"aSprite\" $ do
  'rectangle' 'Polygon' cyan 0 0 10 10
@

This one will have its center of rotation exactly in the middle.

@
'insert' \"aSprite\" $ do
  'rectangle' 'Polygon' magenta (-5) (-5) 10 10
@

You may ask yourself: \"If I can do all my drawing here, then why do
sprites have a rotation, x and y coordinates, or scaling?\"

The main reason is convenience, so you can use the shortcuts of the Sprite monad
instead of overriding your sprite's draw function all the time.
You should make your draw function create a /canonical/ drawing for your sprite.

If you want to do collision detection you will also need to assign a bounding
box to your sprite. A bounding box is the /detectable/ rectangular area that your sprite
occupies on the screen, and it may be independent of the actual image itself.

The is usually demonstrated with circles: If you draw a circle, the corners of its bounding
box will be empty, but objects touching them will be considered as colliding. So in the case
of a circle you may want the bounding box to be a bit smaller than the circle itself, that
way collisions with some outer areas of the circle won't be detected, but /false/ collisions
with areas outside the circle will be less.

You'll have to take my word for it that detecting collisions using boxes is a good tradeoff
between accurracy and performance.

You will find some shortcuts here for easily defining a drawing function and a default
bounding box, but you can also call 'draw' yourself, passing in a drawing function
and then 'setBoundBox'.

Look at "Graphics.Rendering.OpenGL.GL.BeginEnd" for rendering polygons,
and at "Graphics.Rendering.OpenGL.GLU.Quadrics" for circles, spheres and the likes.

Do keep in mind that everything you draw yourself will be seen in collisions from other sprites
but you need to give it a bounding box if you want to check what's colliding with it.
-}

module Graphics.UI.Monono.Drawing (
  -- * Drawing && Bounding Box
  rectangle,
  image,
  disk,
  -- * Just drawing
  drawRectangle,
  drawImage,
  drawDisk,
  point,
  -- * Handy functions for pre-applied colors. Just add the alpha value!
  black, white, red, green, blue, cyan, magenta, yellow,
  renderGame
  )
where

import Graphics.UI.Monono.Types
import Graphics.UI.Monono.Sprite
import Graphics.Rendering.OpenGL hiding (get)
import Graphics.UI.GLUT hiding (get)
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Data.List.Split as DLS
import Control.Monad
import Control.Monad.Trans.State

-- | Shortcut for adding a point when rendering a primitev using 'renderPrimitive'
point :: GLfloat -> GLfloat -> IO ()
point x y = vertex $ Vertex3 x y (0::GLfloat)

white :: GLfloat -> Color4 GLfloat
white = Color4 1 1 1
black :: GLfloat -> Color4 GLfloat
black = Color4 0 0 0
red :: GLfloat -> Color4 GLfloat
red = Color4 1 0 0
green :: GLfloat -> Color4 GLfloat
green = Color4 0 1 0
blue :: GLfloat -> Color4 GLfloat
blue = Color4 0 0 1
cyan :: GLfloat -> Color4 GLfloat
cyan = Color4 0 1 1
magenta :: GLfloat -> Color4 GLfloat
magenta = Color4 1 0 1
yellow :: GLfloat -> Color4 GLfloat
yellow = Color4 1 1 0

-- | Set the drawing to be a rectangle, and the BoundingBox to be the same size
-- and position as the rectangle.
rectangle :: Color a =>
  PrimitiveMode -> a -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Sprite ()
rectangle mode c x y w h = do
  setBoundBox x y w h
  draw $ drawRectangle mode c x y w h

-- | Draws a rectangle using the given OpenGL PrimitiveMode.
-- <x> and <y> are the coordinates of the bottom-left vertex.
drawRectangle :: Color a =>
  PrimitiveMode -> a -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GameData -> IO ()
drawRectangle mode c x y w h gData = do
  renderPrimitive mode $ do
    color c
    point x y
    point x (y+h)
    point (x+w) (y+h)
    point (x+w) y

-- | Set the drawing to be a game image, and the bounding box to be
-- a rectangle that circumscribes it
image :: Int -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Sprite ()
image i x y w h = do
  setBoundBox x y w h
  draw $ drawImage i x y w h

-- | Renders one of the game images.
--
-- The first argument is the /index/ of the desired image in the images list.
--
-- See "Graphics.UI.Monono.Game".'images' to learn how to define the
-- image resources to be referenced here.
drawImage :: Int -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GameData -> IO ()
drawImage imageId x y w h gData = do
  texture Texture2D $= Enabled
  textureBinding Texture2D $= (Just $ (gameTextures gData) !! imageId)
  renderPrimitive Polygon $ do
    color $ white 1
    texCoord2 0.0 0.0
    point x y
    texCoord2 1.0 0.0
    point (x+h) y
    texCoord2 1.0 1.0
    point (x+h) (y+w)
    texCoord2 0.0 1.0
    point x (y+w)
  texture Texture2D $= Disabled
 where
  texCoord2 :: GLdouble -> GLdouble -> IO ()
  texCoord2 x y = texCoord $ TexCoord2 x y

-- | Sets the drawing to a disk, and the boundbox to circumbscribe it.
-- Beware that the center of gravity of the disk defaults to be it's center itself.
-- It's kind of what you want anyways, trust me.
-- 
-- Pass in the same number as the second radius for x and y coordinates to make
-- the center of gravity the bottom left corner instead.
disk :: Color a
  => QuadricDrawStyle -> a -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Sprite ()
disk mode c x y radius radius' = do
  setBoundBox (x-radius') (y-radius') (radius' * 2) (radius' * 2)
  (_,s) <- get
  draw $ drawDisk mode c x y (internalZ s) radius radius'

-- | Draws a disk, or circle. A circle is nothing but a disk filled down to the 0 radius
drawDisk :: Color a
  => QuadricDrawStyle
  -> a -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GameData -> IO ()
drawDisk mode c x y z radius radius' gData = do
  let style = QuadricStyle Nothing NoTextureCoordinates Outside mode
  color c
  preservingMatrix $ do
    translate $ Vector3 x y z
    renderQuadric style $ Disk (realToFrac radius) (realToFrac radius') 25 1

-- | Render the sprite tree to whatever buffer in use (for hit tests or just another frame)
-- Use it if you're an opengl hacker and know what you're doing.
renderGame :: GameData -> IO ()
renderGame gData = do
  let spriteList = DL.sortBy compareZ $ DM.toList $ sprites gData
  case DM.lookup "" $ sprites gData of
    Nothing -> return ()
    Just root -> spriteRenderer spriteList ("", root)
 where
  compareZ a b = compare (internalZ $ snd a) (internalZ $ snd b)
  childrenRenderer sList parent (sPath, sData) =
    when (sPath /= "" && parent == getParent sPath) $ spriteRenderer sList (sPath, sData)
  getParent = DL.concat . (DL.intersperse "/") . DL.init . (DLS.splitOn "/")
  spriteRenderer sList (sPath, sData) = preservingMatrix $ do
    translate $ Vector3 (spriteX sData) (spriteY sData) (internalZ sData)
    rotate (spriteRotation sData) $ Vector3 0 0 1
    scale (spriteScalingX sData) (spriteScalingY sData) 1
    loadName $ Name $ spriteGlName sData
    maybe (return ()) (\f -> f gData) $ spriteDraw sData
    mapM_ (childrenRenderer sList sPath) sList

internalZ :: SpriteData -> GLfloat
internalZ s = case spriteZ s of
  Auto a -> a
  Manual a -> a
