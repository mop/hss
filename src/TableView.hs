import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import UI.HSCurses.Widgets
import Control.Exception
import System
import Data.Char

text :: Int -> String
text num = "something " ++ [ (chr (num + 64)) ]

row :: Int -> Row
row num = [TableCell $ newTextWidget defaultTWOptions (text num) ]

rows :: [Row]
rows = map row [1..10]

tableWidget :: TableWidget
tableWidget = newTableWidget (TBWOptions Nothing None [0] (0, 0)) rows

tableSize :: (Int, Int)
tableSize = (2, 50)
tablePos :: (Int, Int)
tablePos = (2, 0)

activateWidget :: TableWidget -> IO (TableWidget, String)
activateWidget tw = do
    (tw', res) <- tableWidgetActivateCurrent (return ()) tablePos 
                    tableSize DHNormal tw 
    str <- case res of
        Just s  -> return (s)
        Nothing -> return ("")
    return (tw', str)

mainloop :: TableWidget -> String -> IO ()
mainloop tw msg = do
    wMove stdScr 0 0
    drawLine 20 msg
    drawTableWidget tablePos tableSize DHNormal tw
    refresh
    key <- getKey $ return ()
    (tw', msg') <- case key of 
        KeyChar 'q'  -> exitWith ExitSuccess
        KeyChar '\r' -> activateWidget (tw) >>= \(t, m) -> return (t, m)
        KeyChar ' '  -> return (tableWidgetScrollDown tableSize tw, msg)
        KeyChar '-'  -> return (tableWidgetScrollUp tableSize tw, msg)
        KeyChar 'k'  -> return (tableWidgetGoUp tableSize tw, msg)
        KeyChar 'j'  -> return (tableWidgetGoDown tableSize tw, msg)
        KeyChar 'h'  -> return (tableWidgetGoLeft tableSize tw, msg)
        KeyChar 'l'  -> return (tableWidgetGoRight tableSize tw, msg)
        otherwise    -> return (tw, msg)
    mainloop tw' msg'

main :: IO ()
main = do
        start
        cursSet CursorInvisible
        mainloop tableWidget "asdf"
      `finally` end
