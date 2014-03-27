{-# LANGUAGE DoAndIfThenElse  #-}
module My.Demon (
    demonize
    ,killDemon
) where

import System.IO.Error
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types
import System.Posix.IO
import System.Directory
import System.Exit
import Control.Monad
import Data.Char

killDemon :: String -> IO ()
killDemon lockfn = do
    locked <- doesFileExist lockfn
    if not locked then
	putStrLn "Lock file does not exist - if demon is running you must manually kill it"
    else do
	pid <- readFile lockfn
	if (not.null) pid && all isDigit pid then do
	    putStrLn $ "Killing process " ++ pid
	    signalProcess sigTERM (read pid :: CPid)
	    removeFile lockfn
	else  do
	    putStrLn $ "Lock file " ++ lockfn ++ " contains invalid pid"
		       ++ " you must manually kill demon and rm "

demonize :: IO () -> String -> IO ()
demonize mydemon lockfn = do
    forkProcess (child1 mydemon lockfn)
    exitImmediately ExitSuccess

child1 mydemon lockfn = do
    locked <- doesFileExist lockfn
    if (not locked) then do
	createSession
	forkProcess (child2 mydemon lockfn)
	exitImmediately ExitSuccess
    else putStrLn "Demon is running or lockfile must be removed"

child2 mydemon lockfn = do
    --setCurrentDirectory "/"
    mapM_ closeFd [stdInput, stdOutput, stdError]
    nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
    mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
    closeFd nullFd
    pid <- getProcessID
    catchIOError (writeFile lockfn (show pid)) (\e->return ())
    mydemon
