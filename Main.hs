{- 

-}

module Main where

import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Data.Dequeue
import Data.Char

data Pot = Pot ([Int], BankersDequeue Integer)
data GameAttribute = GA Int Int [Pot] [Int]-- Scores, die Behälter, Aktuelle Combo
data GameState = Player Int | Calculation Int

type WasAction a = IOGame GameAttribute () GameState () a

width = 800
height = 600
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

runeNumber = 6
emptyPot = Pot ((zeros runeNumber), empty :: BankersDequeue Integer)

zeros 0 = []
zeros k = 0:zeros (k-1)

--moveRunes_ :: [Pot] -> [Pot] 
--moveRunes_ pots = pots

--moveRunes ::  [Pot] -> Int -> [Pot]
--moveRunes pots field = do
--	let x = pots!!field
--	in newPots <- [if i == field then emptyPot else pots!!i | i <- [0..7]]
--	[]

main :: IO ()
main = do
    let winConfig = ((100,80),(width,height),"WAS")
        gameMap = colorMap 0.0 0.0 0.0 w h
        
        
        initPots =  [emptyPot, emptyPot, emptyPot, emptyPot, emptyPot, emptyPot, emptyPot, emptyPot]
        initCombo = zeros runeNumber
        initGA = GA 0 0 initPots initCombo
        initState = Player 0
        input = [
          (SpecialKey KeyRight, Press, \_ _ -> nextTurn)
          ,(Char (chr 27), Press, \_ _ -> funExit)
          ,(MouseButton LeftButton, StillDown, \_ pos -> lecftClickCallback pos)
          ] 
    funInit winConfig gameMap [] initState initGA input gameCycle (Timer 20) []

lecftClickCallback :: Position -> WasAction ()
lecftClickCallback pos =	return ()

nextTurn :: WasAction ()
nextTurn = do
  gState <- getGameState
  case gState of
    Player n -> do
                  setGameState (Calculation n)
    Calculation n -> do
                  setGameState (Player (1-n))


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

fillPot :: [Int] -> Int -> [Int]
fillPot [] _ = []
fillPot (h:t) 0 = h+1:t
fillPot (h:t) x = h:fillPot t (x-1)