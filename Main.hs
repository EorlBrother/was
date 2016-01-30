{- 

-}

module Main where

import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Data.Dequeue

data GameAttribute = GA Int Int [BankersDequeue Integer] [Int] -- Scores, die Behälter, Aktuelle Combo
data GameState = Player Int | Calculation Int

type WasAction a = IOGame GameAttribute () GameState () a

width = 1920
height = 1080
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

main :: IO ()
main = do
    let winConfig = ((100,80),(width,height),"WAS")
        gameMap = colorMap 0.0 0.0 0.0 w h
        initPots =  [empty :: BankersDequeue Integer, empty :: BankersDequeue Integer, empty :: BankersDequeue Integer
                    , empty :: BankersDequeue Integer, empty :: BankersDequeue Integer, empty :: BankersDequeue Integer]
        initCombo = []
        initGA = GA 0 0 initPots initCombo
        initState = Player 1
        input = [
          (SpecialKey KeyRight, Press, \_ _ -> nextTurn)
          ,(Char 'q',            Press,     \_ _ -> funExit)
          ] 
    funInit winConfig gameMap [] initState initGA input gameCycle (Timer 20) []

nextTurn :: WasAction ()
nextTurn = do
  gState <- getGameState
  case gState of
    Player n -> do
                  setGameState (Calculation n)
    Calculation n -> case n of 
                    1 -> do
                          setGameState (Player 2)
                    2 -> do
                          setGameState (Player 1)  


gameCycle :: WasAction ()
gameCycle = do
  GA s1 s2 pots combo <- getGameAttribute
  printOnScreen (show s1) TimesRoman24 (0,0) 1.0 1.0 1.0
  printOnScreen (show s2) TimesRoman24 (20,0) 1.0 1.0 1.0
  gState <- getGameState
  case gState of
      Player n -> printOnScreen ("Player " ++ show(n)) TimesRoman24 (100,100) 1.0 1.0 1.0
      Calculation n -> printOnScreen ("Calculation " ++ show(n)) TimesRoman24 (100,100) 1.0 1.0 1.0
  showFPS TimesRoman24 (w-60,0) 1.0 0.0 0.0