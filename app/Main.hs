module Main where

import GHC.Arr (Array (Array), Ix (index), array, range)
import Game
import Graphics.Gloss (Color, Display (FullScreen, InWindow), makeColor, play)
import Logic ()
import Rendering (gameAsPicture)
import System.Random (Random (random, randomR, randomRs), StdGen, getStdGen, newStdGen, randomRIO, randomRs)
import System.Random.Shuffle (shuffle')

gameWindow :: Display
gameWindow = InWindow "Bomb Flip" (round screenSize, round screenSize) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 0

-- Funzione per generare la lista casuale
generateList :: StdGen -> [Int]
generateList g = shuffle' (merge zeros (take 18 (randomRs (1, 3) g))) 25 g 
  where zeros = replicate 7 0

-- Funzione per unire 2 liste di numeri
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys 

main :: IO ()
main = do
  gen <- getStdGen
  let numberList = generateList gen
  initialWorld <- loadWorld numberList
  play gameWindow backgroundColor 30 initialWorld gameAsPicture (const id) (const id)
