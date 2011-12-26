{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-|
A 'GameData' holds your 'Sprite' tree and other globals of your game, like image resources
and the window dimensions and title.

Internally, this module handles the setting up of the OpenGL and GLUT facilities,
event dispatching, etc.

-}

module Graphics.UI.Monono.Game
  ( runGame
  , withRoot
  , window
  , images
  , fps
  , background
  )
where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Data.Maybe as DY
import qualified Data.Tuple (uncurry)
import Graphics.Rendering.OpenGL hiding (get)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (get)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.Monono.Loader
import Graphics.UI.Monono.Types
import Graphics.UI.Monono.Sprite
import Graphics.UI.Monono.Drawing
import Graphics.UI.Monono.Collisions
import System

-- | Creates and runs a game, this will usually be at the top level
-- of your program's main function.
runGame :: Game () -> IO ()
runGame game = do
  gameData <- execStateT game $ initialGameData 
  gameRef <- newIORef gameData
  (program,_) <- getArgsAndInitialize

  initialDisplayMode $= [ RGBMode, SingleBuffered, WithAlphaComponent]
  initialWindowSize $= Size (round $ gameWidth gameData) (round $ gameHeight gameData)
  createWindow $ gameName gameData

  blend $= Enabled 
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha) 
  shadeModel $= Flat
  clearColor $= (gameBackground gameData)
  depthFunc $= Just Less
  depthRange $= (0, 1)   -- The default z mapping

  displayCallback $= (display gameRef)
  addTimerCallback (gameTimerMs gameData) (timer gameRef)
  keyboardMouseCallback $= Just (handleInput gameRef)
  reshapeCallback $= Just (reshape gameRef)
  motionCallback $= Just (handleMotion gameRef)
  passiveMotionCallback $= Just (handlePassiveMotion gameRef)

  -- The image assets must be loaded after the window has been created.
  textures <- loadImages $ gameImages gameData
  gameRef $= gameData { gameTextures = textures }

  mainLoop
 where
  initialGameData = GameData
    { sprites = DM.fromList [("", emptySprite "" 1)]
    , glNamesLookup = DM.fromList [(1,"")]
    , glNamesCount = 1
    , gameName = "Hi, I'm Monono"
    , gameWidth = 500
    , gameHeight = 500
    , gameImages = []
    , gameTextures = []
    , gameTimerMs = 20
    , gameSignals = emptySignals
    , gameBackground = Color4 0 0 0 0
    }
  emptySignals = ActiveSignals DM.empty Nothing DM.empty (Modifiers Up Up Up) (Position 0 0) ""

-- | Runs the given 'Sprite' monad in the context of the game's root sprite.
-- This is the entry point to start transversing and manipulating the sprites tree.
withRoot :: Sprite () -> Game ()
withRoot sprite = do
  gData <- get
  case DM.lookup "" $ sprites gData of
    Nothing -> fail $ "How could it be? there is no root sprite? this is impossibru!"
    Just sData -> do
      (gData', sData') <- liftIO $ execStateT sprite (gData, sData)
      put $ gData' { sprites = DM.insert "" sData' $ sprites gData'}

-- | Runs in game setting the window size and name.
-- It's optional, but results are unpredictable if left empty.
window :: String -> GLdouble -> GLdouble -> Game ()
window n w h = modify $ \ g -> g { gameName = n, gameWidth = w, gameHeight = h }

-- | Takes a list of paths to .bmp files that you want to use as textures in your game.
-- The 'InvList' argument lets you set some RGB colors in your bitmap to be treated
-- as transparent. People usually use magenta (255,0,255) as the invisible color
-- because it's easy to remember and not used a lot for actual graphics,
-- except maybe by certain european T elcos that wanted to patent it.
-- If you are such a T elco, or if at the time of this reading they patented it
-- and you can't use it, then you can always use green (0,255,0) which is easy to remember too.
--
-- Btw, the InvList name was kept from the FunGen project, another haskell game framework
-- on which I based to learn how to load textures.
-- 
-- This is how you use it:
-- @
-- images [("yourfile.bmp", Just [(255,0,255)])]
-- @
images :: [(FilePath,InvList)] -> Game ()
images v = modify $ \ g -> g { gameImages = v }

-- | Set your game's framerate.
-- The onTick handler of your Sprites will be called once each time a new frame is rendered.
-- The default is 50 frames per second.
fps :: Int -> Game ()
fps v = modify $ \ g -> g { gameTimerMs = 1000 `div` v}

-- | The window background color. Defaults to black.
-- Cannot be changed at runtime, you may want to use 'draw' on the root sprite
-- to draw something the size of the window and change the color whenever you want.
background :: Color4 GLfloat -> Game ()
background (Color4 r g b a) =
  modify $ \ gd -> gd { gameBackground = Color4 (f r) (f g) (f b) (f a) }
 where
  f = realToFrac

-- What to do when the window is resized
reshape :: IORef GameData -> ReshapeCallback
reshape gameRef size = do
  gData <- readIORef gameRef
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (gameWidth gData) 0 (gameHeight gData)

hitRecordsBufSize :: GLsizei
hitRecordsBufSize = 512

-- Obtains the topmost sprite to be rendered under the current mouse position
-- The root sprite is used when no other sprite was hit.
getMouseHits :: GameData -> Position -> IO [SpriteData]
getMouseHits gData (Position mx my) = do
  vp@(_, (Size _ height)) <- GL.get viewport
  (_, mHitRecords) <- getHitRecords hitRecordsBufSize $ withName (Name 0) $ do
     matrixMode $= Projection
     preservingMatrix $ do
        loadIdentity
        pickMatrix (fromIntegral mx, fromIntegral height - fromIntegral my) (5, 5) vp
        ortho2D 0 (gameWidth gData) 0 (gameHeight gData)
        renderGame gData
     flush
  case mHitRecords of
    Nothing -> return []
    Just hitRecords -> return $ DY.catMaybes $
      (DL.foldl foldHitRecord [] hitRecords) ++ [DM.lookup "" $ sprites gData]
 where
  compareHits (HitRecord z1 _ _) (HitRecord z1' _ _) = compare z1 z1'
  foldHitRecord accum (HitRecord _ _ names) =
    (DL.map lookupByName names) ++ accum
  lookupByName (Name name) = do
    path <- DM.lookup name $ glNamesLookup gData
    DM.lookup path $ sprites gData

-- Global GLUT keyboard and mouse handler, this dispatches to Sprite event handlers.
handleInput :: IORef GameData -> KeyboardMouseCallback
handleInput gameRef mouseBtn@(MouseButton btn) state modifiers mousePos = do
  gData <- readIORef gameRef
  let signals@(ActiveSignals _ _ btns _ _ _) = gameSignals gData
  hitList <- liftIO $ getMouseHits gData mousePos
  case hitList of
    [] -> gameRef $=! gData
            { gameSignals = signals
              { activePosition = mousePos
              , activeModifiers = modifiers
              , activeMouseButtons = if state == Up
                  then DM.delete mouseBtn $ activeMouseButtons signals
                  else activeMouseButtons signals } }
    hits -> do
      -- The event is dispatched to the topmost sprite under the mouse that
      -- has handlers for such events. Sprites that don't have handlers for it
      -- don't receive this type of input.
      let mData = firstHandling hits
            [spriteOnMouseDown, spriteOnMouseUp, spriteOnClick, spriteOnDropIn]
      case mData of
        Nothing -> return ()
        Just sData -> do
          let 
            previousDown = DM.lookup mouseBtn btns
            path = spritePath sData
            sameOld = maybe False (path ==) previousDown
            handlers = if state == Down
              then [spriteOnMouseDown]
              else [spriteOnMouseUp, if sameOld then spriteOnClick else spriteOnDropIn]
            callbacks = sequence $ DY.catMaybes $ map (\f -> f sData) $ handlers
            gData' = gData
              { gameSignals = signals
                { activePosition = mousePos
                , activeModifiers = modifiers
                , activeMouseButtons = if state == Up
                    then DM.delete mouseBtn $ activeMouseButtons signals
                    else DM.insert mouseBtn path $ activeMouseButtons signals } }
          gData'' <- if state == Down
            then runSprite gData' sData callbacks
            else do
              gd <- runSprite gData' sData callbacks
              DY.fromMaybe (return gd) $ do
                exPath <- previousDown
                if (exPath == path)
                  then Nothing
                  else do
                    exData <- DM.lookup exPath $ sprites gData'
                    handler <- spriteOnDropOut exData
                    return $ runSprite gd exData handler

          gameRef $=! gData''
          postRedisplay Nothing

-- Keyboard input: Any Number of keys are allowed to be pressed at the same time.
-- the activeKeys is a map of all the currently pressed keys.
-- The last pressed one also has a special compartment.
-- Keyboard events are propagated to all sprites.
handleInput gameRef key state modifiers mousePos = do
  gData <- readIORef gameRef
  let signals@(ActiveSignals keys _ _ _ _ _) = gameSignals gData

  -- GLUT defaults to calling this handler for every key repetition, so you don't really
  -- know if it's the first time the key was pressed, so we do some tweaking to ignore those.
  if (state == Down) && (DY.isJust $ DM.lookup key keys)
    then return ()
    else do
      let
        gData' = gData
          { gameSignals = signals
            { activeLastKey = Just key
            , activePosition = mousePos
            , activeModifiers = modifiers
            , activeKeys = if state == Up then DM.delete key keys else DM.insert key True keys }}
        toHandle = if state == Up then spriteOnKeyUp else spriteOnKeyDown
        sprite = DL.foldl (foldHandlers toHandle) (return ()) $ DM.toList $ sprites gData'

      gData'' <- execStateT (withRoot sprite) $ gData'
      gameRef $=! gData''
      postRedisplay Nothing
 where
  foldHandlers handler accum (_,sData) = case handler sData of
    Nothing -> accum
    Just handle -> accum >> (with (spritePath sData) handle )

handleMotionGeneric
  :: (SpriteData -> Handler)
  -> (SpriteData -> Handler)
  -> (SpriteData -> Handler)
  -> IORef GameData
  -> MotionCallback
handleMotionGeneric forMove forEnter forLeave gameRef mousePos = do
  gData <- readIORef gameRef
  let signals@(ActiveSignals _ _ _ _ _ lastHovered) = gameSignals gData 
  hitList <- liftIO $ getMouseHits gData mousePos
  case hitList of
    [] -> gameRef $=! gData { gameSignals = signals { activePosition = mousePos }}
    hits -> do
      -- The event is dispatched to the topmost sprite under the mouse that
      -- has handlers for such events. Sprites that don't have handlers for it
      -- don't receive this type of input.
      let mData = firstHandling hits [forMove, forEnter]
      gData' <- case mData of
        Nothing -> return gData
        Just sData -> do
          let
            path = spritePath sData
            sameOld = lastHovered == path
            handlers = sequence $ DY.catMaybes
              [forMove sData, if sameOld then Nothing else forEnter sData]
          if sameOld
            then runSprite gData sData handlers
            else do
              gd <- runSprite gData sData handlers
              DY.fromMaybe (return gd) $ do
                exData <- DM.lookup lastHovered $ sprites gData
                handler <- forLeave exData
                return $ runSprite gd exData handler

      gameRef $=! gData' { gameSignals = signals { activePosition = mousePos }}
      postRedisplay Nothing

handleMotion :: IORef GameData -> MotionCallback  
handleMotion =
  handleMotionGeneric spriteOnDrag spriteOnDragIn spriteOnDragOut

handlePassiveMotion :: IORef GameData -> MotionCallback  
handlePassiveMotion =
  handleMotionGeneric spriteOnMouseMove spriteOnMouseEnter spriteOnMouseLeave

-- The first item of sData who has a handler for any of the given events
firstHandling :: [SpriteData] -> [SpriteData -> Handler] -> Maybe SpriteData
firstHandling sDatas handlers = 
  case DL.filter handles sDatas of
    [] -> Nothing
    f:_ -> Just f
 where 
  handles :: SpriteData -> Bool
  handles sData = DL.any (\f -> DY.isJust $ f sData) handlers

-- Run a given sprite state, updating the global game ref.
runSprite :: GameData -> SpriteData -> Sprite a -> IO GameData
runSprite gData sData sprite = do
  (gData', sData') <- execStateT sprite (gData, sData)
  return $ gData' { sprites = DM.insert (spritePath sData') sData' $ sprites gData' }

-- Timer that triggers the calcualtion of the next frame in the game, and performs a redraw.
timer :: IORef GameData -> TimerCallback
timer gameRef = do
  gData <- readIORef gameRef
  -- Active keys are initially added to the map with a True value, then
  -- set to false once the first frame runs after their execution.
  let
    signals = gameSignals gData
    keys = activeKeys signals
    keysHeld = 0 < (DM.size $ DM.filter not $ keys)
    sprite = DL.foldl (foldHandlers keysHeld) (return ()) $ DM.toList $ sprites gData
    gData' = gData { gameSignals = signals { activeKeys = DM.map (const False) keys } }

  gData'' <- execStateT (withRoot sprite) gData'
  gameRef $=! gData''
  postRedisplay Nothing
  addTimerCallback (gameTimerMs gData'') (timer gameRef)
 where
  foldHandlers keysHeld accum (_,sData) =
    case (spriteOnTick sData, spriteOnKeyHold sData) of
      (Nothing, Nothing) -> accum
      (tick,hold) -> do
        accum
        with (spritePath sData) $ do
          handler tick
          when keysHeld $ handler hold
  handler = DY.fromMaybe (return ())
    
display :: IORef GameData -> DisplayCallback
display gameRef = do
  gData <- readIORef gameRef
  clear [ ColorBuffer]
  renderGame gData
  flush

