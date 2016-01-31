module Util where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL
import System.IO.Unsafe
import System.Random

zeros :: Int -> [Int]
zeros 0 = []
zeros k = 0:zeros (k-1)

genRandomIntIO :: Int -> Int -> IO Int
genRandomIntIO min max = do 
	let x = getStdRandom(randomR (min,max))
	y <- x 
	print y
	x

genRandomInt :: Int -> Int -> Int
genRandomInt min max = (unsafePerformIO (genRandomIntIO min max))

genRandomList :: Int -> Int -> Int -> [Int]
genRandomList 0 _ _= []
genRandomList k min max= (genRandomInt min max):genRandomList (k-1) min max
---- modded from https://github.com/haskell-game/fungen/blob/master/Graphics/UI/Fungen/Objects.hs
--createPicture :: ObjectPicture -> (GameObjectPicture,Point2D)
--createPicture (Basic (Polyg points r g b fillMode))  = (B (P (point2DtoVertex3 points) (Color4 r g b 1.0) fillMode),findSize points)
--createPicture (Basic (Circle radius r g b fillMode)) = (B (C radius (Color4 r g b 1.0) fillMode),(2 * radius,2 * radius))
--createPicture (Tex size picIndex) = (Tx picIndex,size)

-- taken from https://github.com/haskell-game/fungen/blob/master/Graphics/UI/Fungen/Game.hs
loadPictures :: [(FilePath,InvList)] -> IO [TextureObject]
loadPictures pathsAndInvLists = do
        bmps <- loadBitmapList (map pathAndInv2color3List pathsAndInvLists)
        texBmList <- genObjectNames (length bmps)
        texStuff texBmList bmps
        return texBmList

loadPicture :: FilePath -> IO [TextureObject]
loadPicture path = loadPictureInv path Nothing

loadPictureInv :: FilePath -> InvList -> IO [TextureObject]
loadPictureInv f c = loadPictures [(f, c)]