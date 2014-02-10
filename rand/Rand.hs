{-# LANGUAGE BangPatterns  #-}

module My.Rand (
    randi     -- infinite list - same as randoms but returns [Int]
    ,randri   -- infinite list within range - same as randomRs but [Int]
    ,rands    -- strict finite list
    ,randrs   -- strict finite list within range
    ,getStdGen
    ,mkStdGen
    ,randomRs
) where

import System.Random

randi :: StdGen -> [Int]
randi g = randi2 g where
    randi2 g2 = x : randi2 g3 where
        (x,g3) = random g2

randri :: Int -> Int -> StdGen -> [Int]
randri l r g = randri2 g where
    randri2 g2 = x : randri2 g3 where
        (x,g3) = randomR (l,r) g2

rands :: Int -> StdGen -> [Int]
rands !n0 !g0 = rands' n0 g0 [] where
    rands' !0 !g !xs = xs
    rands' !n !g !xs = rands' (n-1) g1 (x:xs) where
        !(!x,!g1) = random g

randrs :: Int -> Int -> Int -> StdGen -> [Int]
randrs !l !r !n0 !g0 = rands' n0 g0 [] where
    rands' !0 !g !xs = xs
    rands' !n !g !xs = rands' (n-1) g1 (x:xs) where
        !(!x,!g1) = randomR (l,r) g

