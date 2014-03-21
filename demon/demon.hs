import System.Environment
import System.Posix.Process
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import My.Demon

mydemon = do
    bracket (openFile "xxx" WriteMode)  hClose $ \h ->
	forever $ do
	    hPutStrLn h "xxx"
	    hFlush h
	    threadDelay (2*10^6)

lockfn = "demon.lcf"

main = do
    args <- getArgs
    case args of
	("stop":xs) -> killDemon lockfn
        _           -> demonize mydemon lockfn
