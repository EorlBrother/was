{- 

-}

module Main where

import Util
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL
import Data.Dequeue
import Data.Char
import Data.IORef
import System.IO.Unsafe

data Rune = Rune Int Int
data Pot = Pot [Int] [Rune]
data GameAttribute = GA Int Int [Pot] [Int] [GameObject ()] (GameObject ())-- Scores, die Behälter, Aktuelle Combo, Runen, BG
data GameState = Player Int | Calculation Int | Init

type WasAction a = IOGame GameAttribute () GameState () a 

width = 800
height = 600
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

runeNumber = 4
emptyPot = Pot (zeros runeNumber) []



magenta :: InvList
magenta = Just [(255,0,255)]

bmpList :: FilePictureList
bmpList = [("graphics/Runenstein1.bmp", magenta),
			("graphics/Runenstein2.bmp", magenta),
			("graphics/Runenstein3.bmp", magenta),
			("graphics/Runenstein4.bmp", magenta),
			("graphics/board.bmp", magenta)]

main :: IO ()
main = do
{-
    let winConfig = ((100,80),(width,height),"WAS")
        gameMap = colorMap 0.0 0.0 0.0 w h
        groups = []
        randomList = genRandomList 16 0 (runeNumber-1)
        initPots =  [randomPot 0 randomList, randomPot 2 randomList, randomPot 4 randomList, randomPot 6 randomList, randomPot 8 randomList, randomPot 10 randomList, randomPot 12 randomList, randomPot 14 randomList]
        initCombo = zeros runeNumber
        initRunes = createInitialRuneObjects randomList
        initBackground = (object "background" (Tex (702, 380) 4) True (400, 380) (0,0) ())
    let initGA = GA 0 0 initPots initCombo initRunes initBackground
        initState = Player 0
        input = [
			(SpecialKey KeyRight, Press, \_ _ -> nextTurn)
			,(Char (chr 27), Press, \_ _ -> funExit)
			,(MouseButton LeftButton, StillDown, \_ pos -> lecftClickCallback pos)
			]
    funInit winConfig gameMap groups initState initGA input gameCycle (Timer 16) bmpList
-}


    let winConfig = ((100,80),(width,height),"WAS")
        gameMap = colorMap 0.0 0.0 0.0 w h
        groups = []
        initPots =  []
        initCombo = zeros runeNumber
        initRunes = []
        initBackground = (object "background" (Tex (702, 380) 4) True (400, 380) (0,0) ())
    let initGA = GA 0 0 initPots initCombo initRunes initBackground
        initState = Init
        input = [
			(SpecialKey KeyRight, Press, \_ _ -> nextTurn)
			,(Char (chr 27), Press, \_ _ -> funExit)
			,(MouseButton LeftButton, StillDown, \_ pos -> lecftClickCallback pos)
			]
    funInit winConfig gameMap groups initState initGA input gameCycle (Timer 16) bmpList

randomPot :: Int -> [Int] -> (WasAction Pot)
randomPot x r= return (initPot 2 x r emptyPot)

initPot :: Int -> Int -> [Int] -> Pot -> Pot
initPot 0 _ _ p = p
initPot x k r p = fillInRune (r!!(k+x-1)) (k+x-1) (initPot (x - 1) k r p)

fillInRune :: Int -> Int -> Pot -> Pot
fillInRune x k (Pot l q) = Pot [if i == x then (l!!i + 1) else l!!i | i <- [0..(runeNumber-1)]] (q ++ [Rune x k])

lecftClickCallback :: Position -> WasAction ()
lecftClickCallback pos =	return ()

moveRunes_ :: [Pot] -> Int  -> [Rune] -> [Pot]
moveRunes_ pots field queue 
  | length queue == 0 = pots
  | otherwise = do
    let Pot oldRunes oldQueue = pots!!field
    let curRune:newBufferQueue = queue
    let newQueue = queue ++ [curRune]
    let Rune curRuneIndex _ = curRune
    let newRunes = [if i == curRuneIndex then (oldRunes!!i + 1) else oldRunes!!i | i <- [0..(runeNumber-1)]]
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

genRandomList :: Int -> Int -> Int -> (WasAction [Int])
genRandomList 0 _ _= return []
genRandomList k min max= do
	tail <- genRandomList (k-1) min max
	head <- randomInt (min, max)
	return (head:tail)


createRuneObject :: Int -> (GLdouble, GLdouble) -> WasAction (GameObject ())
createRuneObject x p =  return (object "rune" (Tex (50, 50) x) True p (0,0) ())

createInitialRuneObjects :: [Int] -> WasAction [GameObject ()]
createInitialRuneObjects r = createInitialRuneObjects_ r 16

createInitialRuneObjects_ :: [Int] -> Int -> WasAction [GameObject ()]
createInitialRuneObjects_ r 0 = return []
createInitialRuneObjects_ r i = do
	head <- createInitialRuneObjects_ r (i-1)
	x <- randomInt (0,800)
	y <- randomInt (0,800)
	tail <- createRuneObject (r!!(i-1)) (realToFrac(x), realToFrac(y))
	return (head ++ [tail])

initGame :: WasAction ()
initGame = do
	randomList <- genRandomList 16 0 (runeNumber-1)
	liftIOtoIOGame (print randomList)
	let	gameMap = colorMap 0.0 0.0 0.0 w h
	let	groups = []
	pot0 <- randomPot 0 randomList
	pot1 <- randomPot 2 randomList
	pot2 <- randomPot 4 randomList
	pot3 <- randomPot 6 randomList
	pot4 <- randomPot 8 randomList
	pot5 <- randomPot 10 randomList
	pot6 <- randomPot 12 randomList
	pot7 <- randomPot 14 randomList
	let initPots = [pot0, pot1, pot2, pot3, pot4, pot5, pot6, pot7]
	let	initCombo = zeros runeNumber
	initRunes <- createInitialRuneObjects randomList
	let	initBackground = (object "background" (Tex (702, 380) 4) True (400, 380) (0,0) ())
	let initGA = GA 0 0 initPots initCombo initRunes initBackground
	setGameAttribute(initGA)
	nextTurn

gameCycle :: WasAction ()
gameCycle = do
  disableGameFlags
  gState <- getGameState
  case gState of
      Init -> initGame
      Player n -> printOnScreen ("Player " ++ show(n)) TimesRoman24 (100,100) 1.0 1.0 1.0
      Calculation n -> printOnScreen ("Calculation " ++ show(n)) TimesRoman24 (100,100) 1.0 1.0 1.0

  GA s1 s2 pots combo runes background<- getGameAttribute
  printOnScreen (show s1) TimesRoman24 (0,0) 1.0 1.0 1.0
  printOnScreen (show s2) TimesRoman24 (20,0) 1.0 1.0 1.0
  showFPS TimesRoman24 (w-60,0) 1.0 0.0 0.0
  drawObject background
  drawObject (runes!!0)
  drawObject (runes!!1)
  drawObject (runes!!2)
  drawObject (runes!!3)
  drawObject (runes!!4)
  drawObject (runes!!5)
  drawObject (runes!!6)
  drawObject (runes!!7)
  drawObject (runes!!8)
  drawObject (runes!!9)
  drawObject (runes!!10)
  drawObject (runes!!11)
  drawObject (runes!!12)
  drawObject (runes!!13)
  drawObject (runes!!14)
  drawObject (runes!!15)