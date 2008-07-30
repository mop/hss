import Control.Monad.State

type IOStack a = StateT [String] IO a

pushStack :: String -> IOStack ()
pushStack str = do
    stack <- get
    put (str:stack)

popStack :: IOStack String
popStack = do
    (x:xs) <- get
    put (xs)
    return x

testStack :: IOStack ()
testStack = do
    pushStack "zomg"
    pushStack "hoho"
    elem <- popStack 
    lift (putStrLn elem)
    pushStack "rofl"
    return ()
    
main :: IO ()
main = do
    (_, stack) <- runStateT testStack [] 
    putStrLn (show stack)
