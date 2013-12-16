module Main (main) where

import Control.Concurrent
import Data.Maybe
import Network
import Network.BSD
import System.Environment
import System.Exit
import System.IO

main = do
    args <- getArgs
    case args of
	[l,u] -> scanRange [] "localhost" [(fromIntegral.read) l .. (fromIntegral.read) u]
	[x,l,u] -> scanRange [] x [(fromIntegral.read) l .. (fromIntegral.read) u]
	[] -> scanRange [] "localhost" [1..256]
	[x] -> scanRange [] x [1..256]
	_ -> usage

usage = do
    hPutStrLn stderr "Usage: Portscan [host] [from_port to_port]"
    exitFailure

pout :: MVar (Maybe PortNumber) -> IO ()
pout mv = do
    mx <- takeMVar mv
    case mx of
	Nothing -> return ()
	Just x  -> showService x >>= putStrLn

scanRange mv host [] = mapM_ pout  mv
scanRange mv host (p:ports) = do
    mvar <- newEmptyMVar
    forkIO (doPort mvar host p)
    scanRange (mvar:mv) host ports

doPort mv host p = do
    scanPort mv host p >>=  putMVar mv

scanPort mvar host port =
    withDefault Nothing (tryPort >> return (Just port))
  where
    tryPort = connectTo host (PortNumber port) >>= hClose

showService port =
    withDefault (show port) $ do
	service <- getServiceByPort port "tcp"
	return (show port ++ " " ++ serviceName service)

withDefault defaultVal action =
    catch action (const $ return defaultVal)

