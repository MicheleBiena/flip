module Main where

import Graphics.Gloss ( play, makeColor, Display(InWindow), Color )
import System.Random ( getStdGen, Random(randomRs), StdGen )
import Game ( maxBombs, screenSize, loadWorld )
import Rendering ( gameAsPicture )
import Logic 
import System.Random.Shuffle ( shuffle' )

gameWindow :: Display
gameWindow = InWindow "Bomb Flip" (round screenSize, round screenSize) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 255

-- Funzione per generare la lista casuale
generateList :: StdGen -> [Int]
generateList g = shuffle' (merge zeros (take (25 - maxBombs) (randomRs (1, 3) g))) 25 g
  where zeros = replicate maxBombs 0

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
  play gameWindow backgroundColor 30 initialWorld gameAsPicture transformGame (const id)
