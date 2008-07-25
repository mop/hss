import UI.HSCurses.Curses

-- Returns the width of the window
width :: IO Int
width = do 
    (h, w) <- scrSize
    return w

-- Returns the height of the window
height :: IO Int
height = do 
    (h, w) <- scrSize
    return h

-- Integer division utility
intDiv :: Int -> Int -> Int
intDiv a b = truncate $ fromIntegral (a) / fromIntegral (b)

-- Returns the width of the message
msgWidth :: String -> IO Int
msgWidth msg = width >>= \w -> return ((intDiv w 2) - (intDiv (length msg) 2))

-- Returns the height of the message
msgHeight :: String -> IO Int
msgHeight _ = height >>= \h -> return (intDiv h 2)

main :: IO ()
main = do
    initCurses
    totalWidth  <- width
    totalHeight <- height
    let msg = "The height of the window is " ++ show totalWidth ++ " " ++ show totalHeight
    myWidth  <- msgWidth msg
    myHeight <- msgHeight msg
    mvWAddStr stdScr myWidth myHeight msg
    refresh
    getch
    endWin
