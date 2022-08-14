module Main where

import Graphics.Gloss
import System.Random
import Game
import Rendering
import Logic
import System.Random.Shuffle ( shuffle' )
import Text.Read (readMaybe)
import Data.Ix
import Control.Monad

levels :: [[[Int]]]
levels = [ -- [quantità 2, quantità 3, quantità bombe]
  [[5, 0, 6], [4, 1, 6], [3, 1, 6], [2, 2, 6], [0, 3, 6]], -- 1
  [[6, 0, 7], [5, 1, 7], [3, 2, 7], [1, 3, 7], [0, 4, 7]], -- 2
  [[7, 0, 8], [6, 1, 8], [4, 2, 8], [2, 3, 8], [1, 4, 8]], -- 3
  [[8, 0, 10], [5, 2, 10], [3, 3, 8], [2, 4, 10], [0, 5, 8]], -- 4
  [[9, 0, 10], [7, 1, 10], [6, 2, 10], [4, 3, 10], [1, 5, 10]], -- 5
  [[8, 1, 10], [5, 3, 10], [3, 4, 10], [2, 5, 10], [0, 6, 10]], -- 6
  [[9, 1, 13], [7, 2, 10], [6, 3, 10], [4, 4, 10], [1, 6, 13]] -- 7 
  ]

gameWindow :: Display
gameWindow = InWindow "Bomb Flip" (round screenSize, round screenSize) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 255

generateList :: StdGen -> Int -> [Int]
generateList gen levelNumber = shuffle' (merge zeros (merge threes(merge twos(replicate (25 - maxBombs - maxThrees - maxTwos) 1)))) 25 gen
    where [maxTwos, maxThrees, maxBombs] = levels!!(levelNumber-1)!!fst (randomR (0,4) gen)
          zeros = replicate maxBombs 0
          twos = replicate maxTwos 2
          threes = replicate maxThrees 3

-- Funzione per unire 2 liste di numeri
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

main :: IO ()
main = do
  gen <- getStdGen
  putStrLn "Inserisci livello di difficoltà 1 - 7"
  level <- getLine
  let levelNumber = read level :: Int
  let numberList = generateList gen levelNumber
  print levelNumber
  print (levels!!(levelNumber - 1)!!fst (randomR (0,4) gen))
  initialWorld <- loadWorld numberList
  play gameWindow backgroundColor 30 initialWorld gameAsPicture transformGame (const id)


