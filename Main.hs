{- 

-}

module Main where

import Util
import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL
import Data.Dequeue
import Data.Char
import Data.IORef
import System.IO.Unsafe
--import Data.Sequence

data Pot = Pot [Int] (BankersDequeue Int)
data GameAttribute = GA Int Int [Pot] [Int] (IO [TextureObject])-- Scores, die Behälter, Aktuelle Combo
data GameState = Player Int | Calculation Int | Init

type WasAction a = IOGame GameAttribute () GameState () a 

width = 800
height = 600
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

runeNumber = 4
emptyPot = Pot (twos runeNumber) (empty :: BankersDequeue Int)

moveRunes_ :: [Pot] -> Int  -> (BankersDequeue Int) -> [Pot]
moveRunes_ pots field queue 
	| Data.Dequeue.null(queue) = pots
	| otherwise = do
		let Pot oldRunes oldQueue = pots!!field
		let Just (curRune, newBufferQueue) = popFront(queue)
		let newQueue = pushBack queue curRune
		let newRunes = [if i == curRune then (oldRunes!!i + 1) else oldRunes!!i | i <- [0..runeNumber]]
		let newPot = Pot newRunes newQueue
		[if i == field then newPot else pots!!i | i <- [0..7]]

moveRunes ::  [Pot] -> Int -> [Pot]
moveRunes pots field = do
	let Pot _ queue = pots!!field
	let	newPots = [if i == field then emptyPot else pots!!i | i <- [0..7]]

	moveRunes_ newPots (field+1) queue

main :: IO ()
main = do
    let winConfig = ((100,80),(width,height),"WAS")
        gameMap = colorMap 1.0 0.0 0.0 w h
        groups = []
        initPots =  [initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot]
        initCombo = twos runeNumber
        initGA = GA 0 0 initPots initCombo initRuneTextures
        initRuneTextures = return ([]::[TextureObject])
        initState = Init
        input = [
          (SpecialKey KeyRight, Press, \_ _ -> nextTurn)
          ,(Char (chr 27), Press, \_ _ -> funExit)
          ,(MouseButton LeftButton, StillDown, \_ pos -> lecftClickCallback pos)
          ] 
    funInit winConfig gameMap groups initState initGA input gameCycle (Timer 16) []

initPot :: Int -> Pot -> Pot
initPot 0 p = p
initPot x p = fillInRune (unsafePerformIO (randInt (0,runeNumber-1))) p

fillInRune :: Int -> Pot -> Pot
fillInRune x (Pot l q) = Pot [if i == x then (l!!i + 1) else l!!i | i <- [0..(runeNumber-1)]] (pushBack q x)

lecftClickCallback :: Position -> WasAction ()
lecftClickCallback pos =	return ()

moveRunes_ :: [Pot] -> Int  -> (BankersDequeue Int) -> [Pot]
moveRunes_ pots field queue 
  | Data.Dequeue.null(queue) = pots
  | otherwise = do
    let Pot oldRunes oldQueue = pots!!field
    let Just (curRune, newBufferQueue) = popFront(queue)
    let newQueue = pushBack queue curRune
    let newRunes = [if i == curRune then (oldRunes!!i + 1) else oldRunes!!i | i <- [0..(runeNumber-1)]]
    let newPot = Pot newRunes newQueue
    [if i == field then newPot else pots!!i | i <- [0..7]]

moveRunes ::  [Pot] -> Int -> [Pot]
moveRunes pots field = do
  let Pot _ queue = pots!!field
  let newPots = [if i == field then emptyPot else pots!!i | i <- [0..7]]
  moveRunes_ newPots (field+1) queue


nextTurn :: WasAction ()
nextTurn = do
  gState <- getGameState  
  case gState of
      Init -> setGameState (Player 0)
      Player n -> setGameState (Calculation n)
      Calculation n -> setGameState (Player (1-n))

checkComboFullfilled :: [Int] -> [Int] -> Bool
checkComboFullfilled [] [] = True
checkComboFullfilled (h1:t1) (h2:t2) = h1 >= h2 && checkComboFullfilled t1 t2
initializeGraphics :: WasAction ()
initializeGraphics = do
	disableGameFlags
	let texture = loadPictureInv "graphics/board.bmp" (Just [(255, 0, 255)])
  	GA s1 s2 pots combo runeTextures<- getGameAttribute
  	setGameAttribute (GA s1 s2 pots combo texture)
  	nextTurn
	return ()

drawSprite :: IO [TextureObject] -> Int -> (GLdouble,GLdouble) -> (GLdouble,GLdouble) -> IO ()
drawSprite picList picIndex (pX, pY) (sX, sY)= do
	loadIdentity
	translate (Vector3 pX pY (0 :: GLdouble) )
	texture Texture2D $= Enabled
	p <- picList
	bindTexture Texture2D (p !! picIndex)
	color (Color4 1.0 1.0 1.0 (1.0 :: GLfloat))
	renderPrimitive Quads $ do
	texCoord2 0.0 0.0;  vertex3 (-x) (-y) 0.0
	texCoord2 1.0 0.0;  vertex3   x  (-y) 0.0
	texCoord2 1.0 1.0;  vertex3   x    y  0.0
	texCoord2 0.0 1.0;  vertex3 (-x)   y  0.0
	texture Texture2D $= Disabled
	where 
		x = sX/2
		y = sY/2

gameCycle :: WasAction ()
gameCycle = do

  GA s1 s2 pots combo runeTextures<- getGameAttribute
  setGameAttribute (GA s1 s2 pots combo runeTextures)
  printOnScreen (show s1) TimesRoman24 (0,0) 1.0 1.0 1.0
  printOnScreen (show s2) TimesRoman24 (20,0) 1.0 1.0 1.0
  gState <- getGameState
  case gState of
      Init -> initializeGraphics
      Player n -> printOnScreen ("Player " ++ show(n)) TimesRoman24 (100,100) 1.0 1.0 1.0
      Calculation n -> printOnScreen ("Calculation " ++ show(n)) TimesRoman24 (100,100) 1.0 1.0 1.0
  GA s1 s2 pots combo runeTextures<- getGameAttribute
  showFPS TimesRoman24 (w-60,0) 1.0 0.0 0.0
  liftIOtoIOGame (drawSprite runeTextures 0 (400,300) (700,300))