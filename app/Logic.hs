module Logic where
import Graphics.Gloss.Interface.IO.Game
    ( Key(SpecialKey),
      KeyState(Down),
      SpecialKey(KeyLeft, KeyUp, KeyRight, KeyDown, KeySpace),
      Event(EventKey) )
import Game
import GHC.Arr((//))
import Rendering (filterWithLambda)

transformGame :: Event -> Game -> Game
transformGame (EventKey (SpecialKey KeyUp) Down _ _) game = moveCursor (-1, 0) game
transformGame (EventKey (SpecialKey KeyRight) Down _ _) game = moveCursor (0, 1) game
transformGame (EventKey (SpecialKey KeyDown) Down _ _) game = moveCursor (1, 0) game
transformGame (EventKey (SpecialKey KeyLeft) Down _ _) game = moveCursor (0, -1) game
transformGame (EventKey (SpecialKey KeySpace) Down _ _) game = tryTurnCell (cursor game) game
-- transformGame (EventKey (Key) Down _ _) game = moveCursor (0, -1) game
transformGame _ game = game

-- SPOSTAMENTO CURSORE

moveCursor :: (Int, Int) -> Game -> Game
moveCursor (x, y) game =
    case state of
        Running -> game {cursor = sommaCursore currentCursor (x, y)}
        _ -> game
        where state = gameState game
              currentCursor = cursor game

sommaCursore :: (Int, Int) -> (Int, Int) -> (Int, Int)
sommaCursore (x1, y1) (x2, y2)
    | xf > 4 = (x1, y1)
    | xf < 0 = (x1, y1)
    | yf > 4 = (x1, y1)
    | yf < 0 = (x1, y1)
    | otherwise = (xf, yf)
    where (xf, yf) = (x1 + x2, y1 + y2)

-- GIRA CASELLE
tryTurnCell :: (Int, Int) -> Game -> Game
tryTurnCell (x, y) game =
    case gameState game of
        NotesTaking -> game
        GameOver -> game {gameState = ShowSolutions}
        GameWon -> game {gameState = ShowSolutions}
        Running -> turnCell game (x,y)
        _ -> game

turnCell :: Game -> (Int, Int) -> Game
turnCell game (x,y)
    | state chosenCell == Covered = game {board = b // [((cellX, cellY), chosenCell{state = Flipped})] }
    | otherwise = game
    where b = board game
          ((cellX, cellY), chosenCell) = head (filterWithLambda (\((xpos, ypos), e) -> xpos == x && ypos == y) b) 