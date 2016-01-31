module Util where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL

zeros 0 = []
zeros k = 0:zeros (k-1)

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