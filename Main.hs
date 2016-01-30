{- 
pong - a very simple FunGEn example.
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2001  Andre Furtado <awbf@cin.ufpe.br>
This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}

module Main where

import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data GameAttribute = Scores Int Int -- Hier können scheinbar VIIIIELE Dinge verpackt werden, s. worms.hs in fungen/examples
data GameState = Player Int | Calculation Int

type WasAction a = IOGame GameAttribute () GameState () a

width = 1920
height = 1080
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

main :: IO ()
main = do
    let winConfig = ((100,80),(width,height),"A brief example!")
        gameMap = colorMap 0.0 0.0 0.0 w h
        initScore = Scores 0 0
        initState = Player 1
        input = [
          (SpecialKey KeyRight, Press, \_ _ -> nextTurn)
          ,(Char 'q',            Press,     \_ _ -> funExit)
          ] 
    funInit winConfig gameMap [] initState initScore input gameCycle (Timer 20) []

{-createBall :: GameObject ()
createBall =
  let ballPic = Basic (Circle 6.0 0.0 1.0 0.0 Filled)
  in object "ball" ballPic False (w/2,h/2) (-8,8) ()

createBar :: GameObject ()
createBar =
  let barBound = [(-25,-6),(25,-6),(25,6),(-25,6)]
      barPic   = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
  in object "bar" barPic False (w/2,30) (0,0) ()

moveBarToRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToRight _ _ = do
  obj     <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 5 <= w)
   then (setObjectPosition ((pX + 5),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

moveBarToLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToLeft _ _ = do
  obj <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 5 >= 0)
    then (setObjectPosition ((pX - 5),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)-}

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
  (Scores s1 s2) <- getGameAttribute
  printOnScreen (show s1) TimesRoman24 (0,0) 1.0 1.0 1.0
  printOnScreen (show s2) TimesRoman24 (20,0) 1.0 1.0 1.0
  gState <- getGameState
  case gState of
      Player n -> printOnScreen ("Player " ++ show(n)) TimesRoman24 (100,100) 1.0 1.0 1.0
      Calculation n -> printOnScreen ("Calculation " ++ show(n)) TimesRoman24 (100,100) 1.0 1.0 1.0
  showFPS TimesRoman24 (w-60,0) 1.0 0.0 0.0