module Rendering where

import GHC.Arr (assocs)
import Game
import Graphics.Gloss
import Graphics.Gloss.Rendering
import qualified Data.Foldable as Map
import Data.IntMap (filterWithKey)

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
        backgroundGrid,
        drawGameFinished state board textures,
        drawCursor pos state
    ]

backgroundGrid :: Picture
backgroundGrid = Color (makeColorI 192 192 192 255) $ Pictures [
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
drawGameFinished GameOver _ _ = drawGameOver
drawGameFinished GameWon _ _= drawGameWon
drawGameFinished ShowSolutions board textures = cellsOfBoard board textures
drawGameFinished _ _ _ = Blank

drawGameOver :: Picture
drawGameOver = Pictures [
    color white $ rectangleSolid 400 100,
    Scale 0.25 0.25 $ Translate (-700) (-30) $ text "Mi dispiace, hai perso"
    ]

drawGameWon :: Picture
drawGameWon = Blank


