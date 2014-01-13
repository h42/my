import My.Log
import Control.Monad
import Control.Concurrent

proc l 0 x = return ()
proc mvar n x = do
    writeLog mvar $ "this is " ++ show (x,n)
    yield
    proc mvar (n-1) x

main = do
    mvar <- openLog "temp"
    mapM_ (\x-> forkIO (proc mvar 3 x )) [1..5]
    putStrLn "Enter 'q' to quit ..." >> getChar
    l <-takeMVar mvar
    putMVar mvar l
    closeLog mvar
    print l

