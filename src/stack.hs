import Control.Monad.State

type StackState a = State [Int] a

testStack :: StackState ()
testStack = do
    stack <- get
    pushAlt 1
    pushAlt 1
    popAlt
    return ()

pushS :: Int -> StackState ()
pushS a = State $ \s -> ((), a:s)

pushAlt :: Int -> StackState()
pushAlt a = do
    state <- get 
    put (a:state)
    return ()

popS :: StackState Int
popS = State $ \s -> (head s, tail s)

popAlt :: StackState Int
popAlt = do
    stack  <- get
    (x:xs) <- return stack
    put xs
    return x

main :: IO ()
main = do
    putStrLn $ show $ runState testStack []
