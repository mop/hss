myRead :: IO String
myRead = getContents >>= \s -> return (s)

main :: IO ()
main = 
    myRead >>= \s ->
    putStrLn s
