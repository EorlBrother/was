{- 

-}

module Main where

import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Data.Dequeue
import Data.Char
import System.IO.Unsafe
--import Data.Sequence

data Pot = Pot [Int] (BankersDequeue Int)
data GameAttribute = GA Int Int [Pot] [Int] -- Scores, die Behälter, Aktuelle Combo
data GameState = Player Int | Calculation Int

type WasAction a = IOGame GameAttribute () GameState () a 

width = 800
height = 600
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

runeNumber = 4
emptyPot = Pot (twos runeNumber) (empty :: BankersDequeue Int)

twos 0 = []
twos k = 2:twos (k-1)

main :: IO ()
main = do
    let winConfig = ((100,80),(width,height),"WAS")
        gameMap = colorMap 0.0 0.0 0.0 w h
        groups = []
        initPots =  [initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot, initPot 2 emptyPot]
        initCombo = twos runeNumber
        initGA = GA 0 0 initPots initCombo
        initState = Player 0
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
    Player n -> do
                  setGameState (Calculation n)
    Calculation n -> do
                  setGameState (Player (1-n))

checkComboFullfilled :: [Int] -> [Int] -> Bool
checkComboFullfilled [] [] = True
checkComboFullfilled (h1:t1) (h2:t2) = h1 >= h2 && checkComboFullfilled t1 t2

gameCycle :: WasAction ()
gameCycle = do
  GA s1 s2 pots combo <- getGameAttribute
  setGameAttribute (GA s1 s2 pots combo)
  printOnScreen (show s1) TimesRoman24 (0,0) 1.0 1.0 1.0
  printOnScreen (show s2) TimesRoman24 (20,0) 1.0 1.0 1.0
  gState <- getGameState
  case gState of
      Player n -> printOnScreen ("Player " ++ show(n)) TimesRoman24 (100,100) 1.0 1.0 1.0
      Calculation n -> printOnScreen ("Calculation " ++ show(n)) TimesRoman24 (100,100) 1.0 1.0 1.0
  showFPS TimesRoman24 (w-60,0) 1.0 0.0 0.0