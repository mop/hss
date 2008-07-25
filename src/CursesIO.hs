module Main
where

import UI.HSCurses.Curses

--main :: IO ()
--main = do
--    initCurses
--    --window <- initScr
--    --window2 <- newWin 30 30 10 10
--    --wAddStr window "Hello World!!"
--    --wAddStr window2 "Other Window!"
--    wAddStr stdScr "Zomg"
--    refresh
--    getCh
--    endWin

main :: IO ()
main = do
    initCurses
    raw True            -- ALL characters are passed to the program 
                        -- (including interrupt commands)
    keypad stdScr True  -- Special keys?
    echo False          -- getch-chars aren't echoed!
    wAddStr stdScr "Print a char!\n"
    refresh
    elem <- getCh
    case elem of
        KeyChar x -> wAddStr stdScr $ x : "\n"
        otherwise -> wAddStr stdScr "another key was pressed\n"
    refresh
    getch
    endWin
