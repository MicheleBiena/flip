module Logic where
import Graphics.Gloss.Interface.IO.Game
import Game
import GHC.Arr((//))
import Rendering (filterWithLambda, gameAsPicture)
import Data.List

transformGame :: Event -> Game -> Game
transformGame (EventKey (SpecialKey KeyUp) Down _ _) game = moveCursor (-1, 0) game
transformGame (EventKey (SpecialKey KeyRight) Down _ _) game = moveCursor (0, 1) game
transformGame (EventKey (SpecialKey KeyDown) Down _ _) game = moveCursor (1, 0) game
transformGame (EventKey (SpecialKey KeyLeft) Down _ _) game = moveCursor (0, -1) game
transformGame (EventKey (SpecialKey KeySpace) Down _ _) game = tryTurnCell (cursor game) game
transformGame (EventKey (Char 'x') Down _ _) game = tryTakeNotes game
transformGame (EventKey (Char '0') Down _ _) game = tryAddNote game 0
transformGame (EventKey (Char '1') Down _ _) game = tryAddNote game 1
transformGame (EventKey (Char '2') Down _ _) game = tryAddNote game 2
transformGame (EventKey (Char '3') Down _ _) game = tryAddNote game 3
transformGame _ game = game

-- SPOSTAMENTO CURSORE

moveCursor :: (Int, Int) -> Game -> Game
moveCursor (x, y) game =
    case state of
        Running  -> game {cursor = sommaCursore currentCursor (x, y)}
        NotesTaking -> game {cursor = sommaCursore currentCursor (x, y)}
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
        GameOver -> flipAll $ game {gameState = ShowSolutions} -- Gira tutte le caselle per mostrare la soluzione
        GameWon -> flipAll $ game {gameState = ShowSolutions}
        Running -> turnCell game (x,y)
        _ -> game

-- Funzione per girare tutte le caselle durante la soluzione
flipAll :: Game -> Game
flipAll game = game {board = flippedBoard}
    where flippedBoard = b // map flipCell (filterWithLambda (\(_, e) -> state e == Covered) b)
          b = board game

-- Rende visibile il contenuto di una singola cella
flipCell :: ((Int, Int), Cell) -> ((Int, Int), Cell)
flipCell (pos, c) = (pos, c {state = Flipped})


-- Cambia lo stato della cella se coperta, altrimenti nulla
turnCell :: Game -> (Int, Int) -> Game
turnCell game (x,y)
    | state chosenCell == Covered = checkGameWon
                                    $ checkGameOver
                                    $ game {board = b // [flipCell((cellX, cellY), chosenCell)] }

    | otherwise = game
    where b = board game
          ((cellX, cellY), chosenCell) = head (filterWithLambda (\((xpos, ypos), e) -> xpos == x && ypos == y) b)


-- Controllo game over
checkGameOver :: Game -> Game
checkGameOver game
    -- Controllo se c'è almeno una bomba girata
    | not (null (filterWithLambda (\(_, e) -> state e == Flipped && content e == Bomb) b)) = game {gameState = GameOver}
    | otherwise = game
    where b = board game

-- Controllo vittoria
checkGameWon :: Game -> Game
checkGameWon game
    | null (filterWithLambda ( \(_,e) -> state e == Covered && content e /= Bomb && content e /= One) b) = game {gameState = GameWon}
    | otherwise = game
    where b = board game

-- GESTIONE NOTE

-- Switch dello stato (Se non in gameover/won)
tryTakeNotes :: Game -> Game
tryTakeNotes game
    | st == Running = game {gameState = NotesTaking}
    | st == NotesTaking = game {gameState = Running}
    | otherwise = game
    where st = gameState game

-- Controllo dello stato prima di inserire una nota
tryAddNote :: Game -> Int -> Game
tryAddNote game note
    | st == NotesTaking = addNote game note
    | otherwise = game
    where st = gameState game

-- Filtra la casella selezionata e invoca flipNote per invertire la data nota
addNote :: Game -> Int -> Game
addNote game intNote = game {board = flipNote notesList note chosenCell b}
    where note = toContent intNote
          (x, y) = cursor game
          b = board game
          chosenCell = head (filterWithLambda (\((xpos, ypos), e) -> xpos == x && ypos == y) b)
          notesList = notes (snd chosenCell)

-- Ricompone il tabellone inserendo al posto giusto una cella con la nota nuova
flipNote :: Notes -> Content -> ((Int, Int), Cell) -> Board -> Board
flipNote notesList note cell board = board // [((cellX, cellY), chosenCell {notes = changedNotes notesList note})]
    where ((cellX, cellY), chosenCell) = cell

-- Se la nota è presente la cancella, altrimenti la inserisce
changedNotes :: Notes -> Content -> Notes
changedNotes notesList note
    | note `elem` notesList = delete note notesList
    | otherwise = insert note notesList
          