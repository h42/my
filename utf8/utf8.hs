{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Char
import Text.Printf
import Prelude hiding (lines)
import qualified Utf8 as U

t_unpack = do
    let ua = map (\i -> chr (0xa0 + i)) [1..10]
	sx = "Hey man, " ++ ua ++ "  ,this is only a test"
	bs2 = U.pack sx
	sx2 = U.unpack bs2
    putStrLn sx
    putStrLn $ sx2 ++ " - len = " ++ (show (length sx2))
	       ++ " - " ++ (show (U.length bs2))

t1 = do
    us <- liftM U.lines (U.readFile "BIG.TXT")
    print $ length us
    let sx = map U.unpack us
	ts = foldl (\acc s ->
	   (foldl (\acc' s' -> acc' + if s'=='q' then 1 else 0  ) 0 s)
	    + acc) 0 sx
    print $ length sx
    print (ts + length sx)


t2 = do
    let ua = map (\i -> chr (0xa0 + i)) [1..10]
	bs = U.pack ua

	ucloop (U.uncons -> Just (c,bs2)) = do
	    putChar c
	    ucloop bs2
	ucloop (U.uncons -> Nothing) = putChar '\n'

    putStrLn ua
    U.putStrLn bs
    ucloop bs

u_uncons = do
    us <- U.readFile "BIG.TXT"
    let
	ucloop (!acc,!n) (U.uncons -> Just (c,bs2)) = do
	    ucloop (acc + if c=='a' then 1 else 0,n+1) bs2
	ucloop (acc,n) (U.uncons -> Nothing) = print acc

    ucloop (0,0) us

main = do
    u_uncons
    t2
    -- t1
    --t_unpack
