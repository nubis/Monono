{-# LANGUAGE OverloadedStrings #-}
{-|
 Collision detection is done among sprites that have bounding boxes.
 To learn more about bound boxes you should look at "Graphics.UI.Monono.Drawing"
 wich covers that and the somewhat related topic of giving your sprites a visual
 representation.

 These functions let you list all the other sprites colliding with the current one,
 and also check for collisions with another sprite in particular.
 
 The path given to these functions does not get checked for existence, so you can
 reference sprites that have not yet been created.

 @
  onTick $ hitting \"../bob\" $ say $ \"I'm hitting my sibling bob\"
 @
  
-}

module Graphics.UI.Monono.Collisions (
  getHits,
  getBoxHits,
  -- | For checking hits with a sprite in particular, you only
  -- | need to pass in the other sprite's path, it may be a relative path.
  isHitting,
  isNotHitting,
  -- | Shorthand versions of the above, but behaving more like a 'when'
  hitting,
  notHitting,
  ) 
where

import qualified Data.Map as DM
import qualified Data.List as DL
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL hiding (get)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (get)
import Graphics.UI.Monono.Types
import Graphics.UI.Monono.Sprite
import Graphics.UI.Monono.Drawing

-- | Gets the paths of all the sprites this sprite is currently hitting.
getHits :: Sprite [String]
getHits = do
  (gData, sData) <- get
  bbox <- getBoundBox
  case bbox of
    Nothing -> return []
    Just (bx, by, bw, bh) -> do
      (gx, gy, gsx, gsy, gr) <- localsToGlobals 0 0
      let 
        lookInHitRecord accum (HitRecord _ _ names) =
          accum ++ (DL.foldl lookupByName [] names)
        lookupByName accum (Name name) =
          maybe accum (:accum) $ DM.lookup name $ glNamesLookup gData

      (_, mHitRecords) <- liftIO $ getBoxHits gData (gx+(bx*gsx)) (gy+(by*gsy)) (bw*gsx) (bh*gsy) gr
      case mHitRecords of
        Nothing -> return []
        Just hitRecords ->
          return $ DL.filter ((spritePath sData) /=) $ DL.foldl lookInHitRecord [] hitRecords 
getBoxHits
  :: GameData -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat
  -> IO ((), Maybe [HitRecord])
getBoxHits gData x y w h rot = do
  vp <- GL.get viewport
  let f = realToFrac
  getHitRecords 512 $ withName (Name 0) $ do
    matrixMode $= Projection
    preservingMatrix $ do
      loadIdentity
      ortho2D 0 (f w) 0 (f h)
      rotate (-rot) $ Vector3 0 0 (1::GLfloat)
      translate $ Vector3 (-x) (-y) (0::GLfloat)
      renderGame gData
      pickMatrix (0, 0) (f w, f h) vp
    flush

isHitting :: String -> Sprite Bool
isHitting other = do
  (_, s) <- get
  hits <- getHits
  return $ DL.any (== (resolvePath (spritePath s) other)) hits

isNotHitting :: String -> Sprite Bool
isNotHitting = (fmap not) . isHitting

hitting :: String -> Sprite () -> Sprite ()
hitting o s = (isHitting o) >>= (flip when s)

notHitting :: String -> Sprite () -> Sprite ()
notHitting o s = (isNotHitting o) >>= (flip when s)
