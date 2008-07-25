module UIUtils (
    SelectionView
   ,newSelectionView
   ,selectionViewMoveUp
   ,selectionViewMoveDown
   ,items
   ,selected
   ,drawSelectionView
   ,inGroupsOf
)
where

import UI.HSCurses.Curses       
import UI.HSCurses.CursesHelper 
import UI.HSCurses.Widgets      

data SelectionView = SelectionView {
    items    :: [String]
   ,selected :: Int
}

-- Returns a new SelectionView with the given items and the given item selected
newSelectionView :: [String] -> Int -> SelectionView
newSelectionView items selected = SelectionView items selected

-- Moves the cursor up
selectionViewMoveUp :: SelectionView -> SelectionView
selectionViewMoveUp view = selectionViewMove view (-1)

-- Moves the cursor down
selectionViewMoveDown :: SelectionView -> SelectionView
selectionViewMoveDown view = selectionViewMove view 1

-- Moves the cursor 
selectionViewMove :: SelectionView -> Int -> SelectionView
selectionViewMove view dir 
    | (length . items) view == 0 = view
    | otherwise                  = SelectionView (items view) newpos
        where   newpos = abs $ ((selected view) + dir) `mod` 
                            (length . items) view

-- Draws the given SelectionView on the given position with the given size
drawSelectionView :: Pos -> Size -> DrawingHint -> SelectionView -> IO ()
drawSelectionView pos size hint view = mapM_ drawElements (zip [0..] elements)
    where   elements      = elementGroups !! 
                        (pageToDisplay view $ itemsPerPage size)
            elementGroups = (items view) `inGroupsOf` (itemsPerPage size)
            width         = fst size
            yCoord        = fst pos
            xCoord        = snd pos
            drawElements (num, elem) 
                | num /= (selectedItemOnPage view (itemsPerPage size)) = do
                    wMove stdScr (yCoord + num) xCoord
                    drawLine width elem
                | otherwise = do
                    wMove stdScr (yCoord + num) xCoord
                    style <- convertStyles [(Style DefaultF DarkCyanB)]
                    withStyle (head style) (drawLine width elem)

-- Returns the items per page from the given size attribute
itemsPerPage :: Size -> Int
itemsPerPage = snd

-- Returns the pages to display from the given SelectionView and the given
-- itemsPerPage
pageToDisplay :: SelectionView -> Int -> Int
pageToDisplay view numItems = intDiv floor (selected view) numItems

-- Returns the number of pages of the given SelectionView and the given drawing
-- size
pages :: SelectionView -> Int -> Int
pages view size = intDiv truncate itemSize size
    where   itemSize = length $ items view

-- Returns the number of the item on the current page which is selected
-- This function takes a SelectionView and the number of items on a page as
-- parameter.
selectedItemOnPage :: SelectionView -> Int -> Int
selectedItemOnPage view numItems = (selected view) -
                                    numItems * (pageToDisplay view numItems)

-- Divides 2 Ints and applies the given conversion function
-- dunno why this declaration don't works :'(
-- intDiv :: (Integral b, RealFrac a) => (a -> b) -> Int -> Int -> Int
intDiv f x y = f $ (fromIntegral x) / (fromIntegral y)

-- This function creates groups of the given list of the given size
-- e.g.:    [1, 2, 3] `inGroupsOf` 2 => [[1, 2], [3]]
--          [1] `inGroupsOf` 2 => [[1]]
--          ...
inGroupsOf :: [a] -> Int -> [[a]]
inGroupsOf xs num = map reverse $ reverse $ snd $ foldl doGrouping (0, [[]]) xs
    where   doGrouping (n, y:ys) x
                | n < num   = (n + 1, (x:y):ys)
                | otherwise = (1, [x] : (y : ys))
