module Game where

import qualified Data.Set as S
import GHC.Arr ( Ix(range), array, Array, (//) )
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

data State = Running | NotesTaking | GameOver | GameWon | ShowSolutions deriving (Eq, Show)


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
    bomb :: Picture,
    one :: Picture,
    two :: Picture,
    three :: Picture,
    noteBomb :: Picture,
    noteOne :: Picture,
    noteTwo :: Picture,
    noteThree :: Picture,
    gameOverText :: Picture,
    gameWonText :: Picture,
    digits :: [Picture]
  }
  deriving (Eq, Show)

maxBombs :: Int
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

mockupBomb :: Cell
mockupBomb = Cell {
  state = Flipped,
  content = Bomb,
  notes = []
} 

mockupNumber :: Cell 
mockupNumber = Cell {
  state = Flipped,
  content = One,
  notes = []
}

mockupNotes :: Cell 
mockupNotes = Cell {
  state = Covered,
  content = Bomb,
  notes = [One, Two, Bomb]
  }

loadWorld :: [Int] -> IO Game
loadWorld numberList = do
  loadedTextures <- loadTextures 
  let initialState = Running
  let initialKeys = S.empty
  let initialCursor = (0,0)
  return
    Game
      { board = array indexRange (zip (range indexRange) (map createCell numberList)) // [((4,4), mockupBomb),
                                                                                         ((0,0), mockupNumber),
                                                                                         ((2,2), mockupNotes)] 
                                                                                         ,
        gameState = initialState,
        cursor = initialCursor,
        gameTextures = loadedTextures,
        keys = initialKeys
      }

loadTextures :: IO Textures
loadTextures = do
  cellTexture <- loadBMP "img/tile.bmp"
  bombTexture <- loadBMP "img/bomb.bmp"
  oneTexture <- loadBMP "img/large_one.bmp"
  twoTexture <- loadBMP "img/large_two.bmp"
  threeTexture <- loadBMP "img/large_three.bmp"
  one_note <- loadBMP "img/note_one.bmp"
  two_note <- loadBMP "img/note_two.bmp"
  three_note <- loadBMP "img/note_three.bmp"
  bomb_note <- loadBMP "img/note_bomb.bmp"
  gameOver <- loadBMP "img/hai perso.bmp"
  gameWon <- loadBMP "img/hai vinto.bmp"
  digit1 <- loadBMP "img/large_one.bmp"
  let digitsTextures = [digit1]
  return Textures {
    cell = cellTexture,
    bomb = bombTexture,
    one = oneTexture,
    two = twoTexture,
    three = threeTexture,
    noteBomb = bomb_note,
    noteOne = one_note,
    noteTwo = two_note,
    noteThree = three_note,
    gameOverText = gameOver,
    gameWonText = gameWon,
    digits = digitsTextures
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
