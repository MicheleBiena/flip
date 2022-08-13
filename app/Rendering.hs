module Rendering where

import GHC.Arr (assocs, numElements)
import Game
import Graphics.Gloss
import Graphics.Gloss.Rendering

gridColor :: Color
gridColor = makeColorI 192 192 192 255 -- Grigio

selectColor :: Color
selectColor = makeColorI 173 255 47 255 -- Green (Normale gioco)

notesColor :: Color
notesColor = makeColorI 255 255 0 255 -- Giallo (Note)

gameAsPicture :: Game -> Picture
gameAsPicture (Game board state pos textures) = Pictures [
        cellsOfBoard board textures, -- Tutte le celle
        backgroundGrid, -- Griglia
        drawCounters board textures, -- Contatori ai lati
        drawGameFinished state board textures, -- Stati di gameover
        drawCursor pos state -- cursore
    ]

-- BACKGROUND

backgroundGrid :: Picture
backgroundGrid = Color gridColor $ Pictures [
        pictures $ map (\n -> Translate (n*r) 0 verticalLine) [-3..3], -- Linee verticali
        pictures $ map (\n -> Translate 0 (n*r)  horizontalLine) [-3..3] -- Linee orizzontali
    ] where r = gridSize

horizontalLine :: Picture
horizontalLine = rectangleSolid (gridSize * 6 + lineWidth) lineWidth 
verticalLine :: Picture
verticalLine = rectangleSolid lineWidth (gridSize * 6 + lineWidth)

-- CURSORE

drawCursor:: (Int, Int) -> Game.State -> Picture
drawCursor pos Running = color selectColor $ Pictures (createSelectSquare pos)
drawCursor pos NotesTaking = color notesColor $ Pictures (createSelectSquare pos)
drawCursor pos _ = Blank

createSelectSquare :: (Int, Int) -> [Picture]
createSelectSquare (x,y) = [
        pictures $ map (\n -> Translate (xpos +  r) (ypos - n * r) $ rectangleSolid squareBase lineWidth) [0,2],
        pictures $ map (\n -> Translate (xpos + n * r) (ypos - r) $ rectangleSolid lineWidth squareBase) [0,2]
    ] where (xpos, ypos) = toAbsolutePos (x, y)
            r = gridSize / 2
            squareBase = gridSize + lineWidth

-- Permette di trasferirsi dalla posizione nella matrice a una posizione assoluta su schermo
toAbsolutePos :: (Int, Int) -> (Float, Float)
toAbsolutePos (x, y) = (fromIntegral y * gridSize - screenSize / 2 + lineWidth / 2,
             fromIntegral (-x) * gridSize + screenSize /2 - lineWidth / 2)

-- TUTTE LE CELLE DEL TABELLONE

-- Stampa tutte le celle secondo il loro stato
cellsOfBoard :: Board -> Textures -> Picture
cellsOfBoard board textures =
        pictures
            $ map (traverseWithGraphics board) [(cell textures, \(_, e) -> state e == Covered), -- coperte
                                                (noteBomb textures, \(_, e) -> state e == Covered && elem Bomb (notes e)), -- nota bomba
                                                (noteOne textures, \(_, e) -> state e == Covered && elem One (notes e)), -- nota 1
                                                (noteTwo textures, \(_, e) -> state e == Covered && elem Two (notes e)), -- nota 2
                                                (noteThree textures, \(_, e) -> state e == Covered && elem Three (notes e)), -- nota 3
                                                (bomb textures, \(_,e) -> state e == Flipped && content e == Bomb), -- Bomba
                                                (one textures, \(_,e) -> state e == Flipped && content e == One), -- 1
                                                (two textures, \(_,e) -> state e == Flipped && content e == Two), -- 2
                                                (three textures, \(_,e) -> state e == Flipped && content e == Three)] -- 3


-- Centra l'immagine nella cella
snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell picture  (row, column) = translate (x + gridSize /2) (y - gridSize/2) picture
    where (x, y) = toAbsolutePos (row, column)

-- Permette di attraversare tutto il tabellone e "incollare" la grafica sulle celle giuste
traverseWithGraphics :: Board -> (Picture, ((Int, Int), Cell) -> Bool) -> Picture
traverseWithGraphics board (pic, predicate) =
    pictures
        $ map (snapPictureToCell pic . fst)
        $ filterWithLambda predicate board

-- Filtra il tabellone secondo una lambda function
filterWithLambda :: (((Int, Int), Cell) -> Bool) -> Board -> [((Int, Int), Cell)]
filterWithLambda predicate board =
    filter predicate $ assocs board

-- DISEGNO STATI GAME OVER

drawGameFinished :: Game.State -> Board -> Textures -> Picture
drawGameFinished GameOver _ textures = gameOverText textures
drawGameFinished GameWon _ textures = gameWonText textures
drawGameFinished ShowSolutions board textures = cellsOfBoard board textures
drawGameFinished _ _ _ = Blank

-- DISEGNO CONTATORI LATERALI 

drawCounters :: Board -> Textures -> Picture
drawCounters board textures = Pictures [
    pictures $ map ((`drawLineCounters` textures) . filterRow board) [0..4], -- Disegna il contatore di ogni riga
    pictures $ map ((`drawLineCounters` textures) . filterColumn board) [0..4] -- Disegna il contatore di ogni colonna
    ]

-- Permette di creare una lista di righe dal tabellone
filterRow :: Board -> Int -> [((Int, Int), Cell)]
filterRow board n = filterWithLambda(\((x,y),_) -> x == n) board

-- Permette di creare una lista di colonne dal tabellone
filterColumn :: Board -> Int -> [((Int, Int), Cell)]
filterColumn board n = filterWithLambda(\((x,y),_) -> y == n) board

-- Permette di disegnare tutti i contatori di una lista di righe o colonne
drawLineCounters :: [((Int, Int), Cell)] -> Textures -> Picture
drawLineCounters positions = drawSingleCounter (xcounter, ycounter) contaNum contaBomb
    where contaNum = sum (map (fromContent . content . snd) positions)
          contaBomb = length (filter (\(_, e) -> content e == Bomb) positions)
          (xcounter, ycounter) = countersPosition (map fst positions)

-- Funzione per capire in che cella disegnare un dato contatore
countersPosition :: [(Int, Int)] -> (Int, Int)
countersPosition positions
    | fst (head positions) == fst (positions!!1) = (fst(head positions), 5)
    | otherwise = (5, snd (head positions))

-- Funzione per disegnare il singolo contatore
drawSingleCounter :: (Int, Int) -> Int -> Int -> Textures -> Picture
drawSingleCounter (x, y) contaNum numBomb textures =
       snapPictureToCell (counterImage contaNum numBomb textures) (x, y)

-- Composizione immagine contatore
counterImage :: Int -> Int -> Textures -> Picture
counterImage numCount bombCount textures = Pictures [
        Translate 0 0 $ counterCell textures, -- sfondo 
        Translate (gridSize/4) (gridSize/4) $ Scale 0.3 0.3 (digitImage digit0), -- digit 0
        Translate (gridSize/18) (gridSize/4) $ Scale 0.3 0.3 (digitImage digit1), -- digit 1 (numero = 1-0)
        Color (makeColorI 0 0 0 255) $ rectangleSolid (gridSize-lineWidth) (lineWidth/2), -- central line
        Translate (-gridSize/5) (-gridSize/4) $ Scale 0.3 0.3 (smallBomb textures), -- bomb image
        Translate (gridSize/4) (-gridSize/4) $ Scale 0.3 0.3 (digitImage bombCount) -- bomb count
    ] where digitImage n = fromNumberToPic n textures
            digit0 = numCount `mod` 10
            digit1
                | numCount < 10 = 0
                | otherwise = 1

-- Convertitore cifra -> texture 
fromNumberToPic :: Int -> Textures -> Picture
fromNumberToPic n textures = digits textures!!n
