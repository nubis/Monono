{- 
Hat Tip to the fungen  guys who dealt with the whole
mess of loading bitmaps as textures.
I did change quite a few things to make it more haskellish,
I think with some little tweaking, this module
could replace the fungen Loader module.

--- This is their licence notice
FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Graphics.UI.Monono.Loader (
  FilePictureList,
  loadImages,
) where

import Graphics.Rendering.OpenGL
import System.IO
import Foreign
import Graphics.UI.Monono.Types
import qualified Data.List as DL
import Control.Monad

type BmpList = [(GLubyte, GLubyte, GLubyte, GLubyte)]
type FilePictureList = [(FilePath,InvList)]
type ColorList3 = [(GLubyte, GLubyte, GLubyte)]

-- | width, height and data of bitmap
type AwbfBitmap = (GLsizei, GLsizei, PixelData GLubyte)

getWH :: String -> IO (GLsizei,GLsizei)
getWH (a:b:c:d:e:f:g:h:_) = do
  return ( (op (bin a) 0) + (op (bin b) 8) + (op (bin c) 16) + (op (bin d) 24)
         , (op (bin e) 0) + (op (bin f) 8) + (op (bin g) 16) + (op (bin h) 24))
 where
  bin x = toBinary(fromEnum x)
  op x n = toDecimal(shiftLeft((DL.replicate 24 '0') ++ ((zeros x) ++ x)) n)
  zeros x = DL.replicate (8 - (length x)) '0'
  shiftLeft a 0 = a
  shiftLeft (_:as) n = shiftLeft(as ++ "0") (n-1)
  shiftLeft _ _ = []

getWH _ = error "Loader.getWH error: strange bitmap file"                    

getBmData :: String -> (GLsizei,GLsizei) -> Maybe ColorList3 -> IO (PixelData GLubyte)
getBmData bmString (bmW,bmH) invList = do
  let colorList = makeColorList bmString (bmW,bmH)
  bmData <- newArray (DL.map makeInvisible colorList)
  return (PixelData RGBA UnsignedByte (castPtr bmData))
 where
  makeInvisible (r,g,b) = case invList of
    Nothing -> Color4 r g b 255
    Just invs -> Color4 r g b $ if (r,g,b) `DL.elem` invs then 0 else 255

makeColorList :: String -> (GLsizei,GLsizei) -> [(GLubyte, GLubyte, GLubyte)]
makeColorList bmString (bmW,bmH) =
  makeColorListAux (bmW `mod` 4) bmString (bmW*bmH) (bmW,bmW)
 where
  makeColorListAux :: GLsizei -> String -> GLsizei -> (GLsizei,GLsizei) -> [(GLubyte, GLubyte, GLubyte)]
  makeColorListAux _ _ 0 _ = []
  makeColorListAux x bmString totVert (0,bmW) =
    makeColorListAux x (drop_ x bmString) totVert (bmW,bmW)
  makeColorListAux x (b:g:r:bmString) totVert (n,bmW) = (ord2 r,ord2 g,ord2 b): (makeColorListAux x bmString (totVert - 1) (n - 1,bmW))
  makeColorListAux _ _ _ _ = error "Loader.makeColorListAux error: strange bitmap file"

-- loads all of the pictures used in the game
loadImages :: [(FilePath,InvList)] -> IO [TextureObject]
loadImages pathsAndInvLists = do
    bmps <- foldM foldBitmap [] (map pathAndInv2color3List pathsAndInvLists)
    texBmList <- genObjectNames (length bmps)
    sequence $ zipWith processTexture texBmList bmps
    return texBmList
 where
  --loads a bitmap from a file
  foldBitmap :: [AwbfBitmap] -> (FilePath, Maybe ColorList3) -> IO [AwbfBitmap]
  foldBitmap accum (bmName, invList) = do
    bmFile <- openBinaryFile bmName (ReadMode)
    bmString <- hGetContents bmFile
    (bmW,bmH) <- getWH $ drop_ 18 bmString
    bmData <- getBmData (drop_ 54 bmString) (bmW,bmH) invList
    hClose bmFile
    return $ accum ++ [(bmW,bmH,bmData)]
  processTexture texture (w,h,bitmap) = do
    textureBinding Texture2D $= (Just texture)
    texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D w h) 0 bitmap
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  pathAndInv2color3List :: (FilePath,InvList) -> (FilePath, Maybe ColorList3)
  pathAndInv2color3List (f,l) = (f, fmap (map toColor3) l)
    where toColor3 (r,g,b) = (ord2 r, ord2 g, ord2 b)

drop_:: Integral b => b -> [a] -> [a]
drop_ b = DL.drop (fromIntegral b)

toDecimal :: String -> GLsizei
toDecimal a = snd $ DL.foldl foldDecimal (32,0) $ reverse a
  where foldDecimal (n, accum) c = (n-1, accum + (if c == '0' then 0 else (2 ^ (32 - n))))
                
toBinary :: Int -> String
toBinary n
        | n < 2 = show n
        | otherwise = toBinary (n `div` 2) ++ (show (n `mod` 2))
        
ord2 :: Enum a => a -> GLubyte
ord2 = toEnum.fromEnum
