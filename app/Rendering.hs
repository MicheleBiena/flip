module Rendering where

import GHC.Arr (assocs)
import Game
import Graphics.Gloss
import Graphics.Gloss.Rendering

gridColor :: Color
gridColor = makeColorI 192 192 192 255

playerColor :: Color
playerColor = makeColorI 50 100 255 255 -- Blue (Vinto)

pcColor :: Color
pcColor = makeColorI 255 50 50 255 -- Red (Perso)

selectColor :: Color
selectColor = makeColorI 173 255 47 255 -- Green (Normale gioco)

notesColor :: Color
notesColor = makeColorI 255 255 0 255 -- Giallo (Note)

gameAsPicture :: Game -> Picture
gameAsPicture (Game board state pos textures _) = Pictures [
        cellsOfBoard board textures,
        drawSingleCounter (0, 5) 0 0 textures,
        --drawSingleCounter (5, 5) 10 0,
        --drawSingleCounter (5, 0) 8 0,
        backgroundGrid,
        drawCounters board textures,
        drawGameFinished state board textures,
        drawCursor pos state
    ]

backgroundGrid :: Picture
backgroundGrid = Color gridColor $ Pictures [
        Translate 0 0 verticalLine,
        Translate r 0 verticalLine,
        Translate (2 * r) 0 verticalLine,
        Translate (3 * r) 0 verticalLine,
        Translate (-r) 0 verticalLine,
        Translate (-2 * r) 0 verticalLine,
        Translate (-3 * r) 0 verticalLine,
        Translate 0 0 horizontalLine,
        Translate 0 r horizontalLine,
        Translate 0 (2 * r) horizontalLine,
        Translate 0 (3 * r) horizontalLine,
        Translate 0 (-r) horizontalLine,
        Translate 0 (-2 * r) horizontalLine,
        Translate 0 (-3 * r) horizontalLine
    ] where r = gridSize

horizontalLine :: Picture
horizontalLine = rectangleSolid (gridSize * 6) lineWidth
verticalLine :: Picture
verticalLine = rectangleSolid lineWidth (gridSize * 6)

drawCursor:: (Int, Int) -> Game.State -> Picture
drawCursor pos Running = color selectColor $ Pictures (createSelectSquare pos)
drawCursor pos NotesTaking = color notesColor $ Pictures (createSelectSquare pos)
drawCursor pos _ = Blank

createSelectSquare :: (Int, Int) -> [Picture]
createSelectSquare (x,y) = [
        Translate (xpos + r) ypos $ rectangleSolid squareBase lineWidth,
        Translate (xpos + r) (ypos - 2 * r) $ rectangleSolid squareBase lineWidth,
        Translate xpos (ypos - r) $ rectangleSolid lineWidth squareBase,
        Translate (xpos + 2 * r) (ypos - r) $ rectangleSolid lineWidth squareBase
    ] where (xpos, ypos) = toAbsolutePos (x, y)
            r = gridSize / 2
            squareBase = gridSize + lineWidth

toAbsolutePos :: (Int, Int) -> (Float, Float)
toAbsolutePos (x, y) = (fromIntegral y * gridSize - screenSize / 2 + lineWidth / 2,
             fromIntegral (-x) * gridSize + screenSize /2 - lineWidth / 2)

cellsOfBoard :: Board -> Textures -> Picture
cellsOfBoard board textures =
        pictures
            $ map (traverseWithGraphics board) [(cell textures, \(_, e) -> state e == Covered),
                                                (noteBomb textures, \(_, e) -> state e == Covered && elem Bomb (notes e)),
                                                (noteOne textures, \(_, e) -> state e == Covered && elem One (notes e)),
                                                (noteTwo textures, \(_, e) -> state e == Covered && elem Two (notes e)),
                                                (noteThree textures, \(_, e) -> state e == Covered && elem Three (notes e)),
                                                (bomb textures, \(_,e) -> state e == Flipped && content e == Bomb),
                                                (one textures, \(_,e) -> state e == Flipped && content e == One),
                                                (two textures, \(_,e) -> state e == Flipped && content e == Two),
                                                (three textures, \(_,e) -> state e == Flipped && content e == Three)]



snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell picture  (row, column) = translate (x + gridSize /2) (y - gridSize/2) picture
    where (x, y) = toAbsolutePos (row, column)

traverseWithGraphics :: Board -> (Picture, ((Int, Int), Cell) -> Bool) -> Picture
traverseWithGraphics board (pic, predicate) =
    pictures
        $ map (snapPictureToCell pic . fst)
        $ filterWithLambda predicate board



filterWithLambda :: (((Int, Int), Cell) -> Bool) -> Board -> [((Int, Int), Cell)]
filterWithLambda predicate board =
    filter predicate $ assocs board

drawGameFinished :: Game.State -> Board -> Textures -> Picture
drawGameFinished GameOver _ textures = gameOverText textures
drawGameFinished GameWon _ textures = gameWonText textures
drawGameFinished ShowSolutions board textures = cellsOfBoard board textures
drawGameFinished _ _ _ = Blank

-- Contatori laterali
drawCounters :: Board -> Textures -> Picture
drawCounters board textures = Blank

drawSingleCounter :: (Int, Int) -> Int -> Int -> Textures -> Picture
drawSingleCounter (x, y) contaNum numBomb textures =
       snapPictureToCell (counterImage contaNum numBomb textures) (x, y)



counterImage :: Int -> Int -> Textures -> Picture
counterImage numCount bombCount textures = Pictures [
        Translate (gridSize/3) (gridSize/4) $ Scale 0.3 0.3 (digitImage digit0), -- digit 0
        Translate (gridSize/5) (gridSize/4) $ Scale 0.3 0.3 (digitImage digit1), -- digit 1 (numero = 1-0)
        Color gridColor $ rectangleSolid gridSize lineWidth, -- central line
        Translate (-gridSize/4) (-gridSize/4) $ Scale 0.25 0.25 (bomb textures), -- bomb image
        Translate (gridSize/3) (-gridSize/4) $ Scale 0.3 0.3 (one textures) -- bomb count
    ] where digitImage n = fromNumberToPic n textures
            digit0 = numCount `mod` 10
            digit1 
                | numCount < 10 = 0
                | otherwise = 1


fromNumberToPic :: Int -> Textures -> Picture
fromNumberToPic n textures = digits textures!!n
