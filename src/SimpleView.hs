import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import UI.HSCurses.Widgets
import Control.Exception
import System

inputWidget :: EditWidget 
inputWidget  = newEditWidget defaultEWOptions ""

done :: IO ()
done = return ()

edit :: EditWidget -> IO EditWidget
edit ew = do 
    (ew', s) <- activateEditWidget done (1, 10) (1, 10) ew
    wMove stdScr 5 0
    drawLine 60 ("saved: " ++ s)
    refresh 
    return ew'

mainloop :: EditWidget -> IO ()
mainloop ew = do  
    c <- getKey done
    ew' <- case c of
        KeyChar 'q' -> exitWith ExitSuccess
        KeyChar 'e' -> edit ew
        _           -> return ew
    mainloop ew'
    
main :: IO ()
main = do
    start
    gotoTop
    drawLine 20 "Hit 'e'!"
    wMove stdScr 1 0
    drawLine 9 "Input: "
    refresh
    cursSet CursorInvisible
    mainloop inputWidget
    `finally` end
