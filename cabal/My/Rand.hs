{-# LANGUAGE BangPatterns  #-}

module My.Rand (
    rand
    ,randr
    ,rands
    ,lazyrands
    ,randrs
    ,lazyRandrs
) where

import System.Random

rand :: StdGen -> (Int,StdGen)
rand g = random g :: (Int,StdGen)

randr  :: Int -> Int -> StdGen -> (Int,StdGen)
randr l r g = randomR (l,r) g :: (Int,StdGen)

rands :: Int -> StdGen -> ([Int],StdGen)
rands !n0 !g0 = rands' n0 g0 [] where
    rands' !0 !g !xs = (xs,g)
    rands' !n !g !xs = rands' (n-1) g1 (x:xs) where
        !(!x,!g1) = random g

lazyrands :: Int -> StdGen -> ([Int],StdGen)
lazyrands n0 g0 = rands' n0 g0 [] where
    rands' 0 g xs = (xs,g)
    rands' n g xs = rands' (n-1) g1 (x:xs) where
        (x,g1) = random g

lazyRandrs :: Int -> Int -> Int -> StdGen -> ([Int],StdGen)
lazyRandrs l r n0 g0 = rands' n0 g0 [] where
    rands' 0 g xs = (xs,g)
    rands' n g xs = rands' (n-1) g1 (x:xs) where
        (x,g1) = randomR (l,r) g

randrs :: Int -> Int -> Int -> StdGen -> ([Int],StdGen)
randrs !l !r !n0 !g0 = rands' n0 g0 [] where
    rands' !0 !g !xs = (xs,g)
    rands' !n !g !xs = rands' (n-1) g1 (x:xs) where
        !(!x,!g1) = randomR (l,r) g

