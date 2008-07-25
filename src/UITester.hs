import qualified UIUtils                    as Utils
import qualified UI.HSCurses.Curses         as Curses
import qualified UI.HSCurses.CursesHelper   as CursesH
import qualified UI.HSCurses.Widgets        as Widgets
import Control.Exception
import System

testData :: [String]
testData = replicate 100 "zomg"

selectionView :: Utils.SelectionView
selectionView = Utils.newSelectionView testData 0

mainloop :: Utils.SelectionView -> IO ()
mainloop view = do
    Utils.drawSelectionView (0, 0) (10, 10) Widgets.DHNormal view
    Curses.refresh
    key <- CursesH.getKey $ return ()
    view' <- case key of
        Curses.KeyChar 'q' -> exitWith ExitSuccess
        Curses.KeyChar 'j' -> return $ Utils.selectionViewMoveDown view
        Curses.KeyChar 'k' -> return $ Utils.selectionViewMoveUp view
        otherwise          -> return view
    mainloop view'

main :: IO ()
main = do
    CursesH.start
    Curses.cursSet Curses.CursorInvisible
    mainloop selectionView
    `finally` CursesH.end
