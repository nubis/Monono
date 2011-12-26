{-# LANGUAGE OverloadedStrings #-}
{-|
Sprites are the most important concept in Monono, check the "Graphics.UI.Monono" docs for
a general description of Sprites.

This module contains functions for creating, modifying a Sprite's attributes and
its event handlers.

-}

module Graphics.UI.Monono.Sprite (
  -- * Creation and transversing
  -- When you insert a new sprite, a 'SpriteData' is created implicitly with default
  -- values for all its attributes. All functions that run in the 'Sprite' monad are
  -- used to read or write to the underlying 'SpriteData'.
  insert,
  with,
  -- * Drawing and BoundingBox
  -- | Check "Graphics.UI.Monono.Drawing" to learn more about these functions
  draw, setBoundBox, getBoundBox, clearBoundBox,
  clearDraw,
  -- * Get, Put, Modify
  -- | Generally you will find yourself reading an attribute just to alter its value
  -- by some pre-defined amount, therefore the naming convention makes the
  -- modifier functions have the short and snappy names.
  --
  -- 'up' and 'down' functions are provided to help composing the /modify/ functions.
  -- They map to + and - with inverted arguments. + probably was not needed
  -- but I added it as a christmas present.

  -- ** Moving
  -- | You can change the /xCoord/ or /yCoord/ properties or use /coords/ to handle both at once.
  coords, getCoords, setCoords,
  xCoord, getXCoord, setXCoord,
  yCoord, getYCoord, setYCoord,

  -- ** Depth
  -- | The z property moves sprites to the front/back. The higher, the closer to the screen
  -- the sprite is rendered.
  --
  -- Monono automatically places your latest created sprite on top, using a z index between
  -- 0.0 and 0.1. You should use values between 1.0 and 2.0 when specifying manually.
  z, getZ, setZ,

  -- ** Scaling
  -- | The /normal/ scaling of your Sprite is 1.0, if you want to make it 50% larger then
  -- your scaling should be 1.5 and so on.
  -- 
  -- You can also set the scaling on each axis independently, but keep in mind that
  -- the 'scaling' family assumes both scalings are the same at all times, so mixing
  -- them with the scaling[X|Y] families is not recommended.
  scaling, getScaling, setScaling,
  scalingX, getScalingX, setScalingX,
  scalingY, getScalingY, setScalingY,

  -- ** Rotating
  -- | Rotation is expressed in degrees.
  rotation, getRotation, setRotation,

  -- ** Extra data, or custom state.
  -- | You may store some extra data with your sprite to do some level of object orientedness.
  -- (I love making those words up)
  --
  -- Your extra data should be 'Typeable', you can store it directly but
  -- when you retrieve it it's wrapped in a Maybe.
  extra, getExtra, setExtra, getExtra_,

  -- * Event handlers
  -- | Sprites listen for events triggered by the user like clicks or keystrokes, and for
  -- events triggered by the game itself, like when a new frame is to be rendered.
  -- 
  -- Event handlers run in the 'Sprite' monad, so they know to which sprite they
  -- are bound, and they can also access other sprites and do things to them.
  -- You can also 'liftIO' to communicate with the real world. 
  --
  -- All event handlers are optional and there are two separate functions for binding
  -- (/on<event>/) and clearing (/clearOn<event>/).
  --
  -- To further simplify input handling, Monono also offers an "Input" module that
  -- lets you query the current state of input inside any sprite. Including keys,
  -- mouse position and modifiers.
  --
  -- Do not be confused by the 'onDrag' family of events dispatched here, they
  -- only represent the gesture of pressing a mouse button, holding it down while
  -- moving the mouse and then releasing it. This behavior does not map directly
  -- to the action of dragging and dropping a Sprite. Check the examples to learn
  -- how to drag and drop a sprite, it's not hard.

  -- ** MouseDown
  -- | A mouse button was pressed while the cursor was over this sprite.
  onMouseDown, clearOnMouseDown,

  -- ** MouseUp
  -- | A mouse button was released while the cursor was over this sprite.
  onMouseUp, clearOnMouseUp,

  -- ** Click
  -- | A mouse button was pressed and then released while the cursor was over this sprite.
  -- If the mouse is dragged out of the sprite, then dragged back in and released,
  -- it still counts as a click.
  onClick, clearOnClick,
  
  -- ** OnMouseMove
  -- | The mouse is being moved around this sprite with no buttons pressed.
  onMouseMove, clearOnMouseMove,

  -- ** OnMouseEnter
  -- | The mouse just entered this sprite with no buttons pressed
  onMouseEnter, clearOnMouseEnter,

  -- ** OnMouseLeave
  -- | Just left the sprite has, pressed its buttons were not.
  onMouseLeave, clearOnMouseLeave,

  -- ** DropIn
  -- | A mouse button was pressed while the cursor was over another sprite and
  -- then it was released while the cursor was over this sprite.
  -- Think about the motion of grabbing something with your mouse, then dropping it
  -- in this sprite.
  onDropIn, clearOnDropIn,

  -- ** DropOut
  -- | A mouse button was pressed on this sprite, then dragged out and
  -- released on another sprite.
  onDropOut, clearOnDropOut,
  
  -- ** OnDrag 
  -- | The mouse is being moved around this sprite with a button pressed down.
  onDrag, clearOnDrag,

  -- ** OnDragIn
  -- | The mouse just entered this sprite with a button pressed down.
  onDragIn, clearOnDragIn,

  -- ** OnDragOut
  -- | The mouse just left for greener pastures and a button was pressed down when he left.
  onDragOut, clearOnDragOut,

  -- ** Key press events: OnKeyDown, OnKeyUp and OnKeyHold
  -- | 'OnKeyDown' and 'OnKeyUp' are triggered when a key is pressed and released respectively.
  --
  -- 'OnKeyHold' is triggered once per frame while one or several keys are being held down.
  --
  -- You can use the functions in Monono's "Input" module to query which key was pressed,
  -- released and which ones are being held down.
  -- 
  -- Also, if one repetition per frame is too ofter for your 'onKeyHold' needs, you can
  -- use a custom counter as your sprite's 'extra' and check which
  -- keys are being pressed on the 'onTick' handler.
  -- 
  -- 'Modifiers' (alt, ctrl, shift) don't trigger Key events, but you can check their state
  -- with the functions on "Input"
  onKeyDown, clearOnKeyDown, onKeyUp, clearOnKeyUp, onKeyHold, clearOnKeyHold,

  -- ** Tick
  -- | The game clocked just ticked, a new frame is to be rendered. A handler for
  -- this event could be used to animate the sprite.
  -- 
  -- For instance, if you want Sprite //crazy-s/ to spin like crazy while growing 
  -- to infinity you can do something like
  --
  -- @
  -- with \"/crazy-s\" $ do
  --   onTick $ do
  --     rotation $ up 10
  --     scaling $ up 1.0
  -- @
  onTick, clearOnTick,

  -- * Misc
  modifySprite,
  getSprite,
  up,
  down,
  localsToGlobals,
  globalsToLocals,
  emptySprite,
  resolvePath,
  say
  ) 
where

import qualified Data.List as DL
import qualified Data.List.Split as DLS
import qualified Data.Map as DM
import qualified Data.Maybe as DY
import Data.Typeable
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL hiding (get)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (get)
import Graphics.UI.Monono.Types

-- | Creates and inserts a new child sprite with the given name.
--
-- The name cannot contain l or . (dot) as I don't think they are valuable
-- enough to compromise the simplicity of the current implementation.
insert :: String -> Sprite a -> Sprite ()
insert path sprite = do
  if "." `DL.isInfixOf` path || "/" `DL.isInfixOf` path
    then fail $ "That's no name for a child!: " ++ path
    else do
      (gData, this) <- get
      let
        glName = (glNamesCount gData) + 1
        newPath = (spritePath this) ++ "/" ++ path
        new = emptySprite newPath glName
        gData' = gData
          { glNamesLookup = DM.insert glName newPath $ glNamesLookup gData
          , glNamesCount = glName
          , sprites = DM.insert newPath new $ sprites gData
          }
      put (gData', this)
      with newPath $ sprite
      return ()

-- Removes a sprite and all it's children from the tree
-- It takes a full path to a sprite and totally destroys it so you better be careful.
-- If by chance you want to destroy the same thing twice this function will be a no-op
-- Not public yet, actually there's no support for destroying sprites but this is
-- here to remind me.
destroy :: String -> Sprite ()
destroy "" = fail "Dont kill the root"
destroy "/" = fail "Dont kill the root"
destroy path = do
  (gData, this) <- get
  let resolved = resolvePath (spritePath this) path
  case DM.lookup resolved $ sprites gData of
    Nothing -> return ()
    Just _ -> do
      let notToDelete = (\k v -> not $ path `DL.isPrefixOf` k)
      put (gData { sprites = DM.filterWithKey notToDelete $ sprites gData }, this)

-- | Lookup another sprite by path and run the given code in its context.
--
-- The lookup is done by a path that can be relative to the current sprite using
-- /../ to move up one level.
-- A path can also start with / denoting an absolute path.
--
-- It fails miserably if the path does not exists.
with :: String -> Sprite a -> Sprite a
with path sprite = do
  (gData, this) <- get
  let resolved = resolvePath (spritePath this) path
  case DM.lookup resolved $ sprites gData of
    Nothing -> fail $ "Sprite not found: "++resolved++" resolved from: "++path
    Just sData -> do
      -- Save the state of 'this' so far, in case childs want to modify it too.
      let gData' = gData { sprites = DM.insert (spritePath this) this $ sprites gData}

      -- Run the child, should  gives us the new 'world' state and it's data so far.
      (r,(gData'', sData')) <- liftIO $ runStateT sprite (gData', sData)

      -- Now we save the latest data from the child
      let gData''' = gData'' { sprites = DM.insert resolved sData' $ sprites gData''}

      -- Lookup whatever changes children may have made to 'this'
      let thisData = DY.fromJust $ DM.lookup (spritePath this) $ sprites gData''
      
      put (gData''', thisData)
      return r

-- Resolve a global path using <path> that may be global or relative to <cwd>
resolvePath :: String -> String -> String
resolvePath cwd path =
  DL.concat $ DL.intersperse "/" $ DL.foldl resolver [] $ DLS.splitOn "/" $ unresolved
 where
  unresolved = case (path, cwd) of
      ("/",_) -> ""
      ("",_) -> "" -- for internal compatibility
      ('/':x:xs, _) -> path
      (_,cwd) -> cwd ++ "/" ++ path
  resolver accum segment = if segment /= ".."
    then accum ++ [segment]
    else if accum == [] then [] else DL.init accum

-- | The draw function runs on IO calling OpenGL rendering instructions.
-- Good news is that "Graphics.UI.Monono.Drawing" provides shortcuts for most common cases.
-- 
-- Most of the time, your drawing function will end up looking like
--
-- @
-- with \"aSprite\" $ do
--   ...
--   draw $ image 0 -10 -10 20 20
-- @
--
-- The draw routine is optional, you may leave it blank and rather use the Sprite as
-- a wrapper of other sprites.
draw :: (GameData -> IO ()) -> Sprite ()
draw v = modifySprite $ \s -> s{ spriteDraw = Just v }
clearDraw :: Sprite ()
clearDraw = modifySprite $ \s -> s{ spriteDraw = Nothing }

setBoundBox :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Sprite ()
setBoundBox x y w h = modifySprite $ \s -> s { spriteBoundBox = Just (x,y,w,h) }
getBoundBox :: Sprite (Maybe BoundBox)
getBoundBox = getSprite spriteBoundBox
clearBoundBox :: Sprite ()
clearBoundBox = modifySprite $ \s -> s { spriteBoundBox = Nothing }

-- | Friendly non-infix version of +
up :: Num a => a -> a -> a
up a b = b + a

-- | Friendly non-infix version of -
down :: Num a => a -> a -> a
down a b = b - a

-- Make a SpriteData with default arguments.
emptySprite :: String -> GLuint -> SpriteData
emptySprite path glName = SpriteData
  { spritePath = path
  , spriteX = 0
  , spriteY = 0
  , spriteZ = Auto $ fromIntegral(glName) / 10000
  , spriteScalingX = 1.0
  , spriteScalingY = 1.0
  , spriteRotation = 0
  , spriteBoundBox = Nothing
  , spriteOnMouseDown = Nothing
  , spriteOnMouseUp = Nothing
  , spriteOnClick = Nothing
  , spriteOnMouseMove = Nothing
  , spriteOnMouseEnter = Nothing
  , spriteOnMouseLeave = Nothing
  , spriteOnDropIn = Nothing
  , spriteOnDropOut = Nothing
  , spriteOnDrag = Nothing
  , spriteOnDragIn = Nothing
  , spriteOnDragOut = Nothing
  , spriteOnKeyDown = Nothing
  , spriteOnKeyUp = Nothing
  , spriteOnKeyHold = Nothing
  , spriteOnTick = Nothing
  , spriteDraw = Nothing
  , spriteGlName = glName
  , spriteExtra = Nothing
  }

-- | Modify the underlying SpriteData
modifySprite :: (SpriteData -> SpriteData) -> Sprite ()
modifySprite f = modify $ \(g,s) -> (g, f s)

-- | Get the underlying SpriteData
getSprite :: (SpriteData -> a) -> Sprite a
getSprite f = fmap ( f . snd) get

coords :: (GLfloat -> GLfloat) -> (GLfloat -> GLfloat) -> Sprite ()
coords xf yf = (xCoord xf) >> (yCoord yf)
getCoords :: Sprite (GLfloat, GLfloat)
getCoords = getSprite $ \ s -> (spriteX s, spriteY s)
setCoords :: GLfloat -> GLfloat -> Sprite ()
setCoords xv yv = (setXCoord xv) >> (setYCoord yv)

xCoord :: (GLfloat -> GLfloat) -> Sprite ()
xCoord f = getXCoord >>= (setXCoord . f)
getXCoord :: Sprite GLfloat
getXCoord = getSprite spriteX
setXCoord :: GLfloat -> Sprite ()
setXCoord v = modifySprite $ \s -> s{ spriteX = v }

yCoord :: (GLfloat -> GLfloat) -> Sprite ()
yCoord f = getYCoord >>= (setYCoord . f)
getYCoord :: Sprite GLfloat
getYCoord = getSprite spriteY
setYCoord :: GLfloat -> Sprite ()
setYCoord v = modifySprite $ \s -> s{ spriteY = v }

z :: (GLfloat -> GLfloat) -> Sprite ()
z f = getZ >>= (setZ . f)
getZ :: Sprite GLfloat
getZ = getSprite $ \s -> case spriteZ s of
  Auto _ -> 0
  Manual v -> (v - 0.5) * 2.0
setZ :: GLfloat -> Sprite ()
setZ v = modifySprite $ \s -> s{ spriteZ = Manual (0.5 + v/2.0) }

scaling :: (GLfloat -> GLfloat) -> Sprite ()
scaling f = (scalingX f) >> (scalingY f)
getScaling :: Sprite GLfloat
getScaling = getScalingX
setScaling :: GLfloat -> Sprite ()
setScaling v = (setScalingX v) >> (setScalingY v)

scalingX :: (GLfloat -> GLfloat) -> Sprite ()
scalingX f = getScalingX >>= (setScalingX . f)
getScalingX :: Sprite GLfloat
getScalingX = getSprite spriteScalingX
setScalingX :: GLfloat -> Sprite ()
setScalingX v = modifySprite $ \s -> s{ spriteScalingX = v }

scalingY :: (GLfloat -> GLfloat) -> Sprite ()
scalingY f = getScalingY >>= (setScalingY . f)
getScalingY :: Sprite GLfloat
getScalingY = getSprite spriteScalingY
setScalingY :: GLfloat -> Sprite ()
setScalingY v = modifySprite $ \s -> s{ spriteScalingY = v }

rotation :: (GLfloat -> GLfloat) -> Sprite ()
rotation f = getRotation >>= (setRotation . f)
getRotation :: Sprite GLfloat
getRotation = getSprite spriteRotation
setRotation :: GLfloat -> Sprite ()
setRotation v = modifySprite $ \s -> s{ spriteRotation = v }

extra :: Typeable a => (a -> a) -> Sprite ()
extra f = getExtra_ >>= maybe (return ()) (setExtra . f)

-- | Returns either the passed in extra value or the default you pass in.
-- It also sets it as the default value if there was nothing set yet.
--
-- If you only use your data in one handler this can save you from having
-- to set it for the first time and instead you can just get it with the default value.
getExtra :: Typeable a => a -> Sprite a
getExtra d = do
  (_,sData) <- get
  case spriteExtra sData of
    Nothing -> do
      setExtra d
      return d
    Just (SpriteExtra v) -> return $ DY.fromMaybe d $ cast v
    
-- | Gets the Maybe value with the extra data for your own use
getExtra_ :: Typeable a => Sprite (Maybe a)
getExtra_ = getSprite $ \s -> (spriteExtra s) >>= (\(SpriteExtra v) -> cast v)

setExtra :: Typeable a => a -> Sprite ()
setExtra v = modifySprite $ \s -> s{ spriteExtra = Just (SpriteExtra v) }

onTick :: Sprite () -> Sprite ()
onTick v = modifySprite $ \s -> s{ spriteOnTick = Just v }
clearOnTick :: Sprite ()
clearOnTick = modifySprite $ \s -> s{ spriteOnTick = Nothing }

onMouseDown :: Sprite () -> Sprite ()
onMouseDown v = modifySprite $ \s -> s{ spriteOnMouseDown = Just v }
clearOnMouseDown :: Sprite ()
clearOnMouseDown = modifySprite $ \s -> s{ spriteOnMouseDown = Nothing }

onMouseUp :: Sprite () -> Sprite ()
onMouseUp v = modifySprite $ \s -> s{ spriteOnMouseUp = Just v}
clearOnMouseUp :: Sprite ()
clearOnMouseUp = modifySprite $ \s -> s{ spriteOnMouseUp = Nothing }

onClick :: Sprite () -> Sprite ()
onClick v = modifySprite $ \s -> s{ spriteOnClick = Just v}
clearOnClick :: Sprite ()
clearOnClick = modifySprite $ \s -> s{ spriteOnClick = Nothing }

onMouseMove :: Sprite () -> Sprite ()
onMouseMove v = modifySprite $ \s -> s{ spriteOnMouseMove = Just v }
clearOnMouseMove :: Sprite ()
clearOnMouseMove = modifySprite $ \s -> s{ spriteOnMouseMove = Nothing }

onMouseEnter :: Sprite () -> Sprite ()
onMouseEnter v = modifySprite $ \s -> s{ spriteOnMouseEnter = Just v }
clearOnMouseEnter :: Sprite ()
clearOnMouseEnter = modifySprite $ \s -> s{ spriteOnMouseEnter = Nothing }

onMouseLeave :: Sprite () -> Sprite ()
onMouseLeave v = modifySprite $ \s -> s{ spriteOnMouseLeave = Just v }
clearOnMouseLeave :: Sprite ()
clearOnMouseLeave = modifySprite $ \s -> s{ spriteOnMouseLeave = Nothing }

onDropIn :: Sprite () -> Sprite ()
onDropIn v = modifySprite $ \s -> s{ spriteOnDropIn = Just v }
clearOnDropIn :: Sprite ()
clearOnDropIn = modifySprite $ \s -> s{ spriteOnDropIn = Nothing }

onDropOut :: Sprite () -> Sprite ()
onDropOut v = modifySprite $ \s -> s{ spriteOnDropOut = Just v }
clearOnDropOut :: Sprite ()
clearOnDropOut = modifySprite $ \s -> s{ spriteOnDropOut = Nothing }

onDrag :: Sprite () -> Sprite ()
onDrag v = modifySprite $ \s -> s{ spriteOnDrag = Just v }
clearOnDrag :: Sprite ()
clearOnDrag = modifySprite $ \s -> s{ spriteOnDrag = Nothing }

onDragIn :: Sprite () -> Sprite ()
onDragIn v = modifySprite $ \s -> s{ spriteOnDragIn = Just v }
clearOnDragIn :: Sprite ()
clearOnDragIn = modifySprite $ \s -> s{ spriteOnDragIn = Nothing }

onDragOut :: Sprite () -> Sprite ()
onDragOut v = modifySprite $ \s -> s{ spriteOnDragOut = Just v }
clearOnDragOut :: Sprite ()
clearOnDragOut = modifySprite $ \s -> s{ spriteOnDragOut = Nothing }

onKeyDown :: Sprite () -> Sprite ()
onKeyDown v = modifySprite $ \s -> s{ spriteOnKeyDown = Just v }
clearOnKeyDown :: Sprite ()
clearOnKeyDown = modifySprite $ \s -> s{ spriteOnKeyDown = Nothing }

onKeyUp :: Sprite () -> Sprite ()
onKeyUp v = modifySprite $ \s -> s{ spriteOnKeyUp = Just v }
clearOnKeyUp :: Sprite ()
clearOnKeyUp = modifySprite $ \s -> s{ spriteOnKeyUp = Nothing }

onKeyHold :: Sprite () -> Sprite ()
onKeyHold v = modifySprite $ \s -> s{ spriteOnKeyHold = Just v }
clearOnKeyHold :: Sprite ()
clearOnKeyHold = modifySprite $ \s -> s{ spriteOnKeyHold = Nothing }

-- | Shortcut for printing a string to stdout from a 'Sprite'
say :: String -> Sprite ()
say = liftIO . putStrLn

-- | A sprite's position, scale and rotation are always relative to its parent.
-- But sometimes you want to know where they stand relative to the window, this function
-- takes a point (x and y) locally relative to the sprite's origin, and
-- returns (xCoord, yCoord, scalingX, scalingY, rotation)
-- where xCoord and yCoord are the position of the same point but relative to the
-- window origin which is at the bottom left corner.
localsToGlobals :: GLfloat -> GLfloat -> Sprite (GLfloat, GLfloat, GLfloat, GLfloat, GLfloat)
localsToGlobals x y = do
  (gData, sData) <- get
  let (transforms, sx, sy, r) = unprojectSprite (sprites gData) (spritePath sData)
  (Vertex3 x y _) <- runMockTransforms transforms gData
  return (realToFrac x, realToFrac y, sx, sy, r)
 where
  runMockTransforms :: IO () -> GameData -> Sprite (Vertex3 GLdouble)
  runMockTransforms transforms gData = liftIO $ preservingMatrix $ do
    loadIdentity
    ortho2D 0 (gameWidth gData) 0 (gameHeight gData)
    m <- GL.get (matrix (Just Projection)) :: IO (GLmatrix GLdouble)
    transforms
    translate $ Vector3 x y (0::GLfloat)
    m' <- GL.get (matrix (Just Projection)) :: IO (GLmatrix GLdouble)
    v <- GL.get viewport
    project (Vertex3 0 0 0) m m' v

-- | The opposite of localsToGlobals, this one translates some global window coordinates
-- into something relative to this sprite.
-- It is for instance used to translate the global window mouse position to local sprite ones.
globalsToLocals :: GLfloat -> GLfloat -> Sprite (GLfloat, GLfloat, GLfloat, GLfloat, GLfloat)
globalsToLocals x y = do
  (gData, sData) <- get
  let (transforms, sx, sy, r) = unprojectSprite (sprites gData) (spritePath sData)
  (Vertex3 x' y' _) <- runMockTransforms transforms gData
  return (realToFrac x', realToFrac y', sx, sy, r)
 where
  runMockTransforms :: IO () -> GameData -> Sprite (Vertex3 GLdouble)
  runMockTransforms transforms gData = liftIO $ preservingMatrix $ do
    matrixMode $= Projection
    v <- GL.get viewport
    loadIdentity
    ortho2D 0 (gameWidth gData) 0 (gameHeight gData)
    m <- GL.get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLdouble)
    transforms
    m' <- GL.get (matrix (Just Projection)) :: IO (GLmatrix GLdouble)
    unProject (Vertex3 (realToFrac x) (realToFrac y) 0) m m' v

-- In order to unproject a sprite we need to reproduce al the transformations
unprojectSprite :: (DM.Map String SpriteData) ->
  String -> (IO (), GLfloat, GLfloat, GLfloat)
unprojectSprite allSprites target =
  DM.foldlWithKey foldParents ((return ()),1,1,0) allSprites
 where
  foldParents accum@(transform,scalex,scaley,rot) path s = if path `DL.isPrefixOf` target
    then 
      ( ( transform >> (makeTransform s) )
      , scalex * spriteScalingX s
      , scaley * spriteScalingY s
      , rot + spriteRotation s
      )
    else accum
  makeTransform s = do
    translate $ Vector3 (spriteX s) (spriteY s) 0
    rotate (spriteRotation s) $ Vector3 0 0 (1::GLfloat)
    scale (spriteScalingX s) (spriteScalingY s) 1
