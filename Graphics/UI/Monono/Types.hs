{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.UI.Monono.Types
  ( GameData(..)
  , Game
  , SpriteData(..)
  , Sprite
  , SpriteExtra(..)
  , BoundBox(..)
  , InvList
  , ActiveSignals(..)
  , ZIndex(..)
  , Handler
  )
where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import qualified Data.Map as DM
import Data.Typeable
import Control.Monad.Trans.State

-- | Which colors to be considered 'invisible' on a loaded bitmap, if any.
-- Expressed as RGB.
type InvList = Maybe [(Int,Int,Int)]

-- | GameData is the root state.
-- It is created implicitly and populated indirectly by the methods in "Graphics.UI.Monono.Game"
data GameData = GameData
  { sprites :: DM.Map String SpriteData
  , glNamesLookup :: DM.Map GLuint String
  , glNamesCount :: GLuint
  , gameName :: String
  , gameWidth :: GLdouble
  , gameHeight :: GLdouble
  , gameImages :: [(FilePath, InvList)]
  , gameTextures :: [TextureObject]
  , gameTimerMs :: Int
  , gameSignals :: ActiveSignals
  , gameBackground :: Color4 GLclampf
  }

data ActiveSignals = ActiveSignals
  { activeKeys :: DM.Map Key Bool
  , activeLastKey :: Maybe (Key)
  , activeMouseButtons :: DM.Map Key String
  , activeModifiers :: Modifiers
  , activePosition :: Position
  , activeLastHoveredSprite :: String
  }

-- | A SpriteData holds the state of a Sprite in the game.
-- Values of this type are created when you call "Graphics.UI.Monono.Sprite"'insert'
-- It can also contain arbitrary extra data (as long as it's Typeable)
data SpriteData = SpriteData
  { spritePath :: String
  , spriteX :: GLfloat
  , spriteY :: GLfloat
  , spriteZ :: ZIndex
  , spriteScalingX :: GLfloat
  , spriteScalingY :: GLfloat
  , spriteRotation :: GLfloat
  , spriteBoundBox :: Maybe BoundBox
  , spriteOnMouseDown :: Handler
  , spriteOnMouseUp :: Handler
  , spriteOnClick :: Handler
  , spriteOnMouseMove :: Handler
  , spriteOnMouseEnter :: Handler
  , spriteOnMouseLeave :: Handler
  , spriteOnDropIn :: Handler
  , spriteOnDropOut :: Handler
  , spriteOnDrag :: Handler
  , spriteOnDragIn :: Handler
  , spriteOnDragOut :: Handler
  , spriteOnKeyDown :: Handler
  , spriteOnKeyUp :: Handler
  , spriteOnKeyHold :: Handler
  , spriteOnTick :: Handler
  , spriteDraw :: Maybe (GameData -> IO ())
  , spriteGlName :: GLuint
  , spriteExtra :: Maybe SpriteExtra
  }

type Handler = Maybe (Sprite ())
type BoundBox = (GLfloat, GLfloat, GLfloat, GLfloat)

data ZIndex = Auto GLfloat | Manual GLfloat

data SpriteExtra = forall s. (Typeable s) => SpriteExtra s

type Game = StateT GameData IO

-- | The sprite monad holds as its state the 'GameData' and current 'SpriteData'
type Sprite = StateT (GameData, SpriteData) IO

