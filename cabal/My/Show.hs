module My.Show (
    ishow
    ,fshow
) where

import Data.Char

convi :: (Integral a) => (Int,String) -> a -> (Int,String)
convi (l,xs) i
    | i >= 10 = convi (l+1,xs') q
    | otherwise = (l,xs')
  where (q,r) = quotRem i 10
	r' = fromIntegral r :: Int
	xs' = (chr $ r' + ord '0') : xs

ishow :: (Integral a) => Int -> a -> String
ishow l i = s where
    posi = abs i
    (l',s') = convi (1,[]) posi
    (l2,s2) = if posi == i then (l',s') else (l'+1,'-':s')
    s = if l > l2 then replicate (l-l2) ' ' ++ s2 else s2

fshow :: (RealFrac a) => (Int,Int) -> a -> String
fshow (l,res) rnum
    | res > 0 = s
    | otherwise = ishow l (round rnum)
  where
    posi = abs rnum
    (l1,s1) = convi (1,[]) (round $ posi * 10 ^ (abs res) )
    zpad = "0." ++ replicate (res-l1) '0'
    (l2,s2) | l1 <= res = (length zpad + l1, zpad ++ s1)
	    | otherwise = (l1+1, take (l1-res) s1 ++ "." ++ drop (l1-res) s1)
    (l3,s3) = if posi == rnum then (l2,s2) else (l2+1,'-':s2)
    s = if l > l3 then replicate (l-l3) ' ' ++ s3 else s3
