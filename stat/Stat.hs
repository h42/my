module Stat (
    freq
    ,Freq
    ,mean
    ,mode
    ,popMean
    ,pmf
    ,stdDev
    ,stdDev1
    ,variance
    ,variance1
    ,popVariance
    ,popStdDev

    ,binomial
    ,binomials
    ,combinations
) where

import Data.List
import qualified Data.Map as Map


data Freq a = Freq Int (Map.Map a Int) deriving Show

freq :: Ord a => [a] -> Freq a
freq xs = Freq i m where
    f (i',m') x =  (i'+1, Map.insertWith (+) x 1 m')
    (i,m) = foldl' f (0,Map.empty) xs

pmf :: (Ord a, Floating b) => a -> Freq a -> b
pmf k (Freq tcnt fm) = case Map.lookup k fm of
    Just cnt -> fromIntegral cnt / fromIntegral tcnt
    _ -> 0

--
-- Mode
--
mode [] = error "empty list in call to mode"
mode (ikey:xs) = ans where
    Freq n m = freq xs
    ans=Map.foldlWithKey f [(ikey,0)] m
    f ((k,v):as) key x =
        if x>v then [(key,x)]
        else if x<v then ((k,v):as)
        else ((key,x):(k,v):as)
--
-- Mean
--
mean :: Fractional a => [a] -> a
mean xs = a' / n' where (a',n') = foldl' (\(a,n) x -> (a+x,n+1)) (0,0) xs

-- sum of x * P(x)
popMean :: [Double] -> [Double] -> Double
popMean xs ps = sum $ zipWith (*) xs ps

--
-- Infinite population variation using probability of x and x^2
--
popVariance xs ps = x2' - u'*u' where
    (x2',u') = foldl' (\(x2,u) (x,p) -> (x2+p*x*x, u+p*x)) (0,0 ) (zip xs ps)

popStdDev xs ps = sqrt $ popVariance xs ps

--
-- Variance divided by n using formula = (sum (x^2) / n) - (mean^2)
--
stdDev :: Floating a => [a] -> a
stdDev xs = sqrt $ variance xs

variance :: Floating a => [a] -> a
variance xs = (x2 / n - u*u) where
    (n,u'',x2) = foldl' (\(n',u',x2') x -> (n'+1, u'+x, x2'+x*x)) (0,0,0) xs
    u = u'' / n

--
-- Sample variance and standard deviation  dividing by (n-1)
-- using speedup formula - (sum (x^2) - (mean^2)/n ) / (n-1)
--
stdDev1 :: Floating a => [a] -> a
stdDev1 xs = sqrt $ variance1 xs

--
variance1 :: Floating a => [a] -> a
variance1 xs = (x2 - x1*x1/n) / (n-1) where
    (n,x1,x2) = foldl' (\(n',x1',x2') x -> (n'+1, x1'+x, x2'+x*x)) (0,0,0) xs

--
-- COMBINATIONS
--
combinations :: Int -> Int -> Int
combinations n x
    | n <= x = 1
    | otherwise = quot (product [n,n-1..n-x+1]) (product [1..x])

binomial :: Int -> Int -> Double -> Double
binomial n x p = p^x * (1-p)^(n-x) * (fromIntegral $ combinations n x)

-- cumulative binomial for x = lb to ub
binomials n x1 x2 p = sum $ map (\x -> binomial n x p) [x1..x2]

