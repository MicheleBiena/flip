module Rendering where

import GHC.Arr (assocs)
import Game
import Graphics.Gloss
import Graphics.Gloss.Rendering

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
        -- drawCells board,
        backgroundGrid,
        -- drawGameOver state
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
    ] where (xpos, ypos) = (fromIntegral y * gridSize - screenSize / 2 + lineWidth / 2,
             fromIntegral (-x) * gridSize + screenSize /2 - lineWidth / 2)
            r = gridSize / 2
            squareBase = gridSize + lineWidth


