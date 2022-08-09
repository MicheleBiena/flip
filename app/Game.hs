module Game where

import qualified Data.Set as S
import GHC.Arr (Array, Ix (range), array, newSTArray)
import Graphics.Gloss (Picture, loadBMP, Point)
import Graphics.Gloss.Interface.IO.Interact (Key)
import System.Random (StdGen, mkStdGen, newStdGen, randomR, randomRIO)

data Game = Game
  { board :: Board,
    gameState :: State,
    cursor :: (Int, Int),
    gameTextures :: Textures,
    keys :: S.Set Key
  }
  deriving (Eq, Show)

data State = Running | NotesTaking | GameOver Winner deriving (Eq, Show)

data Winner = Player | PC deriving (Eq, Show)


type Board = Array (Int, Int) Cell

data Cell = Cell
  { content :: Content,
    state :: CellState,
    notes :: Notes
  }
  deriving (Eq, Show)

data Content = Bomb | One | Two | Three deriving (Eq, Show)

data CellState = Flipped | Covered deriving (Eq, Show)

type Notes = [Content]

data Textures = Textures
  { cell :: Picture,
    bomb :: Picture
  }
  deriving (Eq, Show)

maxBombs :: Integer
maxBombs = 7

gridSize :: Float
gridSize = 120

lineWidth :: Float
lineWidth = 10

screenSize :: Float
screenSize = gridSize * 6 + lineWidth

n :: Int
n = 5 -- Tabella 5 x 5

indexRange :: ((Int, Int), (Int, Int))
indexRange = ((0, 0), (n - 1, n - 1))

loadWorld :: [Int] -> IO Game
loadWorld numberList = do
  cellTexture <- loadBMP "img/knight-front.bmp"
  bombTexture <- loadBMP "img/knight-front.bmp"
  let initialState = NotesTaking
  let initialKeys = S.empty
  let loadedTextures =
        Textures
          { cell = cellTexture,
            bomb = bombTexture
          }
  return
    Game
      { board = array indexRange $ zip (range indexRange) (map createCell numberList),
        gameState = initialState,
        cursor = (0,1),
        gameTextures = loadedTextures,
        keys = initialKeys
      }

toContent :: Int -> Content
toContent 0 = Bomb
toContent 1 = One
toContent 2 = Two
toContent 3 = Three
toContent _ = Bomb

createCell :: Int -> Cell
createCell number =
  Cell
    { content = toContent number,
      state = Covered,
      notes = []
    }
