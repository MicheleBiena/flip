module Game where

import qualified Data.Set as S
import GHC.Arr ( Ix(range), array, Array, (//) )
import Graphics.Gloss (Picture, loadBMP, Point)
import Graphics.Gloss.Interface.IO.Interact (Key)
import System.Random (StdGen, mkStdGen, newStdGen, randomR, randomRIO)
import Text.ParserCombinators.ReadP (count)

data Game = Game
  { board :: Board,
    gameState :: State,
    cursor :: (Int, Int),
    gameTextures :: Textures
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
    counterCell :: Picture,
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
    smallBomb :: Picture,
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
  let initialCursor = (0,0)
  return
    Game
      { board = array indexRange (zip (range indexRange) (map createCell numberList)) {-// [{-((4,4), mockupBomb),-}
                                                                                         ((0,0), mockupNumber){-,
                                                                                         ((2,2), mockupNotes) -}
                                                                                        ]-},
                                                                                        
        gameState = initialState,
        cursor = initialCursor,
        gameTextures = loadedTextures
      }

loadTextures :: IO Textures
loadTextures = do
  cellTexture <- loadBMP "img/casella.bmp"
  counterCellTexture <- loadBMP "img/casella_contatore.bmp"
  bombTexture <- loadBMP "img/bomba_casella.bmp"
  oneTexture <- loadBMP "img/uno_casella.bmp"
  twoTexture <- loadBMP "img/due_casella.bmp"
  threeTexture <- loadBMP "img/tre_casella.bmp"
  one_note <- loadBMP "img/nota_uno.bmp"
  two_note <- loadBMP "img/nota_due.bmp"
  three_note <- loadBMP "img/nota_tre.bmp"
  bomb_note <- loadBMP "img/nota_bomba.bmp"
  gameOver <- loadBMP "img/hai perso.bmp"
  gameWon <- loadBMP "img/hai vinto.bmp"
  smallBombTexture <- loadBMP "img/bomba.bmp"
  digit0 <- loadBMP "img/zero.bmp"
  digit1 <- loadBMP "img/uno.bmp"
  digit2 <- loadBMP "img/due.bmp"
  digit3 <- loadBMP "img/tre.bmp"
  digit4 <- loadBMP "img/quattro.bmp"
  digit5 <- loadBMP "img/cinque.bmp"
  digit6 <- loadBMP "img/sei.bmp"
  digit7 <- loadBMP "img/sette.bmp"
  digit8 <- loadBMP "img/otto.bmp"
  digit9 <- loadBMP "img/nove.bmp"
  let digitsTextures = [digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9]
  return Textures {
    cell = cellTexture,
    counterCell = counterCellTexture,
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
    smallBomb = smallBombTexture,
    digits = digitsTextures
  }

toContent :: Int -> Content
toContent 0 = Bomb
toContent 1 = One
toContent 2 = Two
toContent 3 = Three
toContent _ = Bomb

fromContent :: Content -> Int
fromContent Bomb = 0
fromContent One = 1
fromContent Two = 2
fromContent Three = 3

createCell :: Int -> Cell
createCell number =
  Cell
    { content = toContent number,
      state = Covered,
      notes = []
    }
