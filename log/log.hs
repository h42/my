import My.Log
import Control.Monad
import Control.Concurrent
import System.IO.Error

proc l 0 x = return ()
proc mvar n x = do
    writeLog mvar $ "this is " ++ show (x,n)
    yield
    proc mvar (n-1) x

main = do
    rc <- tryIOError (openLog "temp")
    case rc of
      Left e -> putStrLn $ show e
      Right mvar ->
        if False then do -- CONcurrent test
            mapM_ (\x-> forkIO (proc mvar 3 x )) [1..5]
            putStrLn "Enter 'q' to quit ..." >> getChar
            l <-takeMVar mvar
            putMVar mvar l
            closeLog mvar
        else do -- NON concurrent test
            mapM (writeLog mvar) $ map show [1..5]
            closeLog mvar

