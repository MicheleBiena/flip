module Rendering where

import GHC.Arr (assocs)
import Game
import Graphics.Gloss
import Graphics.Gloss.Rendering
import qualified Data.Foldable as Map

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
        notesOfBoard board state textures,
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
    ] where (xpos, ypos) = toAbsolutePos (x, y)
            r = gridSize / 2
            squareBase = gridSize + lineWidth

toAbsolutePos :: (Int, Int) -> (Float, Float)
toAbsolutePos (x, y) = (fromIntegral y * gridSize - screenSize / 2 + lineWidth / 2,
             fromIntegral (-x) * gridSize + screenSize /2 - lineWidth / 2)


drawCells :: Board -> Cell -> Picture -> Picture
drawCells board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

cellsOfBoard :: Board -> Textures -> Picture
cellsOfBoard board textures = Pictures [
       -- drawCells board (Cell Bomb Covered []) (cell textures),
        --drawCells board (Cell One Covered []) (cell textures),
        --drawCells board (Cell Two Covered []) (cell textures),
        -- drawCells board (Cell Three Covered []) (cell textures),
        pictures 
            $ map (traverseWithGraphics board) [(cell textures, Nothing)],
        drawCells board (Cell Bomb Flipped []) (bomb textures),
        drawCells board (Cell One Flipped []) (one textures),
        drawCells board (Cell Two Flipped []) (two textures),
        drawCells board (Cell Three Flipped []) (three textures)
    ]


snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell picture  (row, column) = translate (x + gridSize /2) (y - gridSize/2) picture
    where (x, y) = toAbsolutePos (row, column)

notesOfBoard :: Board -> Game.State -> Textures -> Picture
notesOfBoard _ Running _ = Blank
notesOfBoard _ GameOver _ = Blank
notesOfBoard _ GameWon _ = Blank
notesOfBoard board NotesTaking textures =
    pictures 
    $ map (traverseWithGraphics board) [(cell textures, Nothing),
        (noteBomb textures, Just Bomb),
        (noteOne textures, Just One),
        (noteTwo textures, Just Two),
        (noteThree textures, Just Three)
        ]


traverseWithGraphics :: Board -> (Picture, Maybe Content) -> Picture
traverseWithGraphics board (pic, Just content) =    
    pictures
        $ map (snapPictureToCell pic . fst)                      
        $ filter (\(_, e) -> state e == Covered && elem content (notes e))
        $ assocs board
traverseWithGraphics board (pic, Nothing) = 
    pictures
        $ map (snapPictureToCell pic . fst)                      
        $ filter (\(_, e) -> state e == Covered)
        $ assocs board

