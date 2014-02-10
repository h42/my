module My.Log (
    Log (..)
    ,openLog
    ,writeLog
    ,closeLog
    ) where

import System.IO
import System.Directory
import Control.Concurrent.MVar
import My.Time

-- MUST putMVar after every takeMVar

data Log = Log {
    zh ::Handle
    ,zfn :: String
    ,zmaxlines :: Int
    ,zlines :: Int
    ,zopen :: Bool
    } deriving Show

openLog fn = do
    let maxlines = 10
    h <- openFile fn WriteMode
    newMVar $ Log h fn maxlines 0 True

closeLog m = do
    l <- takeMVar m
    hClose (zh l)
    putMVar m l

writeLog :: MVar Log -> String -> IO ()
writeLog mvar str = do
    (yr,mon,day,hr,minute,sec,_) <- ltime
    let s' = show (yr,mon,day,hr,minute,sec) ++ " " ++ str
    l <- takeMVar mvar
    hPutStrLn (zh l) s'
    hFlush (zh l)
    putMVar mvar $ l{zlines=zlines l+1}

{-
main = do
    l <- openLog "temp"
    closeLog l
-}
