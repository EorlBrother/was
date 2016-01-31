import System.IO.Unsafe
import System.Random

main :: IO ()
main = do 
        printStuff
        printStuff
  
genRandomIntIO :: Int -> Int -> IO Int
genRandomIntIO min max = getStdRandom(randomR (min,max))

genRandomInt :: Int -> Int -> Int
genRandomInt min max = (unsafePerformIO (genRandomIntIO min max))

randomInt :: Int
randomInt = genRandomInt 0 10

printStuff :: IO ()
printStuff = do 
              randomInt <- genRandomIntIO 0 10 
              print $ show $ randomInt
              randomInt <- genRandomIntIO 0 10
              print $ show $ randomInt
              randomInt <- genRandomIntIO 0 10
              print $ show $ randomInt
              randomInt <- genRandomIntIO 0 10
              print $ show $ randomInt
              randomInt <- genRandomIntIO 0 10
              print $ show $ randomInt

              