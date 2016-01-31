module Util where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL
import System.IO.Unsafe
import System.Random

zeros :: Int -> [Int]
zeros 0 = []
zeros k = 0:zeros (k-1)

genRandomIntIO_pure :: Int -> Int -> IO Int
genRandomIntIO_pure min max = do 
	let x = getStdRandom(randomR (min,max))
	y <- x 
	print y
	x

genRandomInt_pure :: Int -> Int -> Int
genRandomInt_pure min max = (unsafePerformIO (genRandomIntIO_pure min max))

genRandomList_pure :: Int -> Int -> Int -> [Int]
genRandomList_pure 0 _ _= []
genRandomList_pure k min max= (genRandomInt_pure min max):genRandomList_pure (k-1) min max

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