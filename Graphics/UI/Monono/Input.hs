{-# LANGUAGE OverloadedStrings #-}
{-|
Last known state of user input can always be accessed on the 'Sprite' monad.

All these methods have several versions, I guess that most commonly
you would like to use the ones starting with /when/, which behave similarly
to the 'when' in "Control.Monad", executing the passed in do block only
if that given input is active.

For representing inputs this module uses (and re-exports) the types used by GLUT
-}

module Graphics.UI.Monono.Input (
  -- * Keys Down, Keys Up
  -- | Monono tracks all keys being held down at any given time, so you can easily do
  -- input handling for simultaneous key presses.
  -- All functions take a 'Key' but shortcuts ending with _ are available for some of them
  -- taking only a 'Char' or 'String' depending on which case.
  --
  -- Look at the documentation for 'Key'
  getPressedKeys, keyIsDown, isDown, isDown_, allKeysAreDown, anyKeysAreDown,
  allDown, allDown_, anyDown, anyDown_,
  keyIsUp, isUp, allKeysAreUp, anyKeysAreUp,
  allUp, anyUp,

  -- * Last Pressed Key
  -- | Monono keeps track of all the keys the user is holding down, so you can do multi
  -- input processing. But in some cases you just need to know which one of the
  -- currently pressed keys was the last one added, these methods help you query that key.
  -- The ultra-mega shortcut 'key' is provided, it acts as whenKeyIsLast so you can write
  -- @
  -- onKeyDown $ key 'a' $ say "You pressed a"
  -- @
  keyIsLast, whenKeyIsLast, getLastKey, key, key_,

  -- * Modifiers 
  -- | These are for checkig if a given modifier is pressed. But keep in mind
  -- modifiers don't trigger events on themselves. You cannot make your spaceship
  -- fire by just pressing the ctrl key. It's annoying I know, but look at the bright side
  -- , if you can find one.
  ctrlIsModifying, withCtrl, withoutCtrl, shiftIsModifying, withShift, withoutShift,
  altIsModifying, withAlt, withoutAlt,

  -- * Mouse Position
  -- | The last known mouse 'Position'
  getMousePos,
  getGlobalMousePos
  ) 
where

import qualified Data.Map as DM
import qualified Data.List as DL
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL hiding (get)
import Graphics.UI.GLUT hiding (get)
import Graphics.UI.Monono.Types
import Graphics.UI.Monono.Sprite

-- Get the underlying signals in the game state
getSignals :: (ActiveSignals -> a) -> Sprite a
getSignals f = fmap (f . gameSignals . fst) get

getPressedKeys :: Sprite [Key]
getPressedKeys = getSignals (DM.keys . activeKeys)

keyIsDown :: Key -> Sprite Bool
keyIsDown k = fmap (k `DL.elem`) getPressedKeys

-- | Specially for Chars, so you don't have to compose your own 'Key'
-- | If you want to use any non 'Char', check 'isDown_'
isDown_ :: Char -> Sprite () -> Sprite ()
isDown_ c s = isDown (Char c) s

isDown :: Key -> Sprite () -> Sprite ()
isDown k s = (keyIsDown k) >>= (flip when s)

allDown_ :: String -> Sprite () -> Sprite ()
allDown_ ks s = allDown (map Char ks) s

allDown :: [Key] -> Sprite () -> Sprite ()
allDown ks s = (allKeysAreDown ks) >>= (flip when s)

anyDown_ :: String -> Sprite () -> Sprite ()
anyDown_ ks s = anyDown (map Char ks) s

anyDown :: [Key] -> Sprite () -> Sprite ()
anyDown ks s = (anyKeysAreDown ks) >>= (flip when s)

allKeysAreDown :: [Key] -> Sprite Bool
allKeysAreDown ks = getPressedKeys >>= \ps -> return (DL.all (`DL.elem` ps) ks)

anyKeysAreDown :: [Key] -> Sprite Bool
anyKeysAreDown ks = getPressedKeys >>= \ps -> return (DL.any (`DL.elem` ps) ks)

keyIsUp :: Key -> Sprite Bool
keyIsUp k = fmap not $ keyIsDown k

-- | Spares you from creating a 'Key' value if you only want to use one 'Char' 
isUp_ :: Char -> Sprite () -> Sprite ()
isUp_ k s = isUp (Char k) s

isUp :: Key -> Sprite () -> Sprite ()
isUp k s = (keyIsUp k) >>= (flip when s)

allKeysAreUp :: [Key] -> Sprite Bool
allKeysAreUp ks = fmap not $ anyKeysAreDown ks

anyKeysAreUp :: [Key] -> Sprite Bool
anyKeysAreUp ks = fmap not $ allKeysAreDown ks

allUp :: [Key] -> Sprite () -> Sprite ()
allUp ks s = (allKeysAreUp ks) >>= (flip when s)

anyUp :: [Key] -> Sprite () -> Sprite ()
anyUp ks s = (anyKeysAreUp ks) >>= (flip when s)

getLastKey :: Sprite (Maybe Key)
getLastKey = getSignals activeLastKey

keyIsLast :: Key -> Sprite Bool
keyIsLast k = fmap (maybe False (k==)) getLastKey

whenKeyIsLast :: Key -> Sprite () -> Sprite ()
whenKeyIsLast k b = (keyIsLast k) >>= (flip when b)

key :: Key -> Sprite () -> Sprite ()
key = whenKeyIsLast

key_ :: Char -> Sprite () -> Sprite ()
key_ k = whenKeyIsLast (Char k)

modDown :: (Modifiers -> KeyState) -> Sprite Bool
modDown l = getSignals $ (==Down) . l . activeModifiers

altIsModifying :: Sprite Bool
altIsModifying = modDown alt

shiftIsModifying :: Sprite Bool
shiftIsModifying = modDown shift

ctrlIsModifying :: Sprite Bool
ctrlIsModifying = modDown ctrl

withMod :: (Modifiers -> KeyState) -> Sprite () -> Sprite ()
withMod f s = (modDown f) >>= (flip when s)

withCtrl :: Sprite () -> Sprite () 
withCtrl = withMod ctrl

withAlt :: Sprite () -> Sprite () 
withAlt = withMod alt

withShift :: Sprite () -> Sprite () 
withShift = withMod shift 

withoutMod :: (Modifiers -> KeyState) -> Sprite () -> Sprite ()
withoutMod f s = (fmap not (modDown f)) >>= (flip when s)

withoutCtrl:: Sprite () -> Sprite ()
withoutCtrl = withoutMod ctrl

withoutAlt :: Sprite () -> Sprite ()
withoutAlt = withoutMod alt

withoutShift :: Sprite () -> Sprite ()
withoutShift = withoutMod shift

-- | Gets the global mouse position, that is, respective to the window origin.
-- Deals with the known issue of window managers reporting an inverted Y coordinate.
-- (or at least works on OSX)
getGlobalMousePos :: Sprite (GLfloat, GLfloat)
getGlobalMousePos = do
  height <- fmap (realToFrac . gameHeight . fst) get
  (Position x y) <- getSignals activePosition
  return (fromIntegral x, height - (fromIntegral y))

-- | Gets the mouse position relative to the origin of this sprite
getMousePos :: Sprite (GLfloat, GLfloat)
getMousePos = do
  (x,y) <- getGlobalMousePos
  (x',y',_,_,_) <- globalsToLocals x y
  return (x', y')

