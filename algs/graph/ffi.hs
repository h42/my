{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Foreign
import Foreign.C.Types

n = 20000000

p1 = do
    ip <- mallocArray n :: IO (Ptr CInt)
    p2 ip (0::Int)

p2 ip i
    | i>=n = return ip
    | otherwise = do
        pokeElemOff ip i (fromIntegral $ i*2)
        p2 ip (i+1)

for i n f
    | i<n = do
        f i
        for (i+1) n f
    | otherwise = return ()

pm = do
    ip <- mallocArray n :: IO (Ptr CInt)
    --for 0 n (\ i -> pokeElemOff ip i (fromIntegral $ i*2))
    forM_ [1..n] (\ i -> pokeElemOff ip i (fromIntegral $ i*2))
    return ip

main = do
    ip <- pm
    peekElemOff ip (n-5) >>= print
