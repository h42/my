{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Utf8 (
    lines
    ,unlines
    ,pack
    ,unpack
    ,uncons

    ,B.length
    ,BC.putStrLn
    ,B.readFile
    ,B.writeFile

) where

import Data.List hiding (lines,unlines)
import Prelude   hiding (lines,unlines,head,tail)
--import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word
import Data.Char
import Data.Bits

default (Int)

-----------------------
-- TAKE / DROP
-----------------------
c2bpos :: Int -> B.ByteString -> Int  -- get byte pos of char n
c2bpos n bs
    | n < 0 = -1
    | otherwise = c2bpos' 0 0 n (B.length bs) bs

c2bpos' :: Int -> Int -> Int -> Int -> B.ByteString -> Int
c2bpos' cpos bpos n blen bs
    | bpos >= blen       = -1
    | cpos >= n          = bpos - 1
    | w .&. 0xc0 == 0x80 = c2bpos' cpos (bpos+1) n blen bs
    | otherwise          = c2bpos' (cpos+1) (bpos+1) n blen bs
  where w = B.index bs bpos

-----------------------
-- LINES
-----------------------
lines :: B.ByteString -> [B.ByteString]
lines ps
    | B.null ps = []
    | otherwise = case B.elemIndex 10 ps of
	 Nothing -> [ps]
	 Just n  -> B.take n ps : lines (B.drop (n+1) ps)

unlines :: [B.ByteString] -> B.ByteString
unlines [] = B.empty
unlines ss = (B.concat $ intersperse nl ss) `B.append` nl
	     where nl = B.singleton 10

-----------------------
-- PACK
-----------------------
pack :: String -> B.ByteString
pack s = B.pack $ concatMap packChar s

packChar :: Char -> [Word8]
packChar c
    | oc <= 0x7f   = [oc]
    | oc <= 0x7ff  = [ 0xc0 + (oc `shiftR` 6)
			, 0x80 + oc .&. 0x3f ]
    | oc <= 0xffff = [ 0xe0 + (oc `shiftR` 12)
			, 0x80 + ((oc `shiftR` 6) .&. 0x3f)
			, 0x80 + oc .&. 0x3f ]
    | otherwise    = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
			, 0x80 + oc .&. 0x3f ]
  where oc = fromIntegral $ ord c

-----------------------
-- UNPACK
-----------------------
unpack :: B.ByteString -> String
unpack bs | B.null bs = ""
unpack bs = s where (_,_,s) = B.foldr' unpack2 (0,0,[]) bs

unpack2 :: Word8 -> (Int,Int,String) -> (Int,Int,String)
unpack2 !w' (!acc,!bytes,!str)
    | w < 0x80 = (0,0,(chr w : str))
    | w < 0xc0 = ( ((acc `shiftL` 6) + (w .&. 0x3f)), bytes+1, str)
    | ismb2 && bytes == 1 = (0, 0, (mbyte2 : str))
    | ismb3 && bytes == 2 = (0, 0, (mbyte3 : str))
    | ismb4 && bytes == 3 = (0, 0, (mbyte4 : str))
    | otherwise = (0,0,(badchar : str))
    where
	w = fromEnum w'
	mbyte2 = chr $ ((w .&. 0x1f) `shiftL` 6) + acc
	mbyte3 = chr $ ((w .&. 0x0f) `shiftL` 12) + acc
	mbyte4 = chr $ ((w .&. 0x07) `shiftL` 18) + acc
	ismb2 = w.&.0xe0 == 0xc0
	ismb3 = w.&.0xf0 == 0xe0
	ismb4 = w.&.0xf8 == 0xf0

-----------------------
-- UNCONS
-----------------------
badchar = '\xfffd'

uncons :: B.ByteString -> Maybe (Char, B.ByteString)
uncons (B.null -> True) = Nothing
uncons (B.uncons -> Just (w, bs))
    | w < 0x80 = Just (chr $ fromEnum w, bs)
    | w < 0xc0 = Just (badchar, bs)
    | ismb2    = uncons2 w' bs
    | ismb3    = uncons3 w' bs
    | ismb4    = uncons4 w' bs
    | otherwise = Just (badchar, bs)
    where
	w' = fromEnum w
	ismb2 = w'.&.0xe0 == 0xc0 && B.length bs >= 1
	ismb3 = w'.&.0xf0 == 0xe0 && B.length bs >= 2
	ismb4 = w'.&.0xf8 == 0xf0 && B.length bs >= 3

uncons2 w bs =
    if acc >= 0 then  Just (chr $ ((w .&. 0x1f) `shiftL` 6) + acc, B.tail bs)
    else Just (badchar, bs)
    where w2 = fromEnum $ B.head bs
	  acc = if w2 .&. 0xc0 == 0x80 then  w2 .&. 0x3f
		else -1

uncons3 w bs =
    if acc >= 0 then
	Just (chr $ ((w .&. 0x0f) `shiftL` 12) + acc, B.drop 2 bs)
    else Just (badchar, bs)
    where
	w2 = fromEnum $ B.head bs
	w3 = fromEnum $ B.index bs 1
	acc = if chker w2 && chker w3 then  (w2 .&. 0x3f) + (w3 .&. 0x3f)
	      else -1

uncons4 w bs =
    if acc >= 0 then
	Just (chr $ ((w .&. 0x07) `shiftL` 18) + acc, B.drop 3 bs)
    else Just (badchar, bs)
    where
	w2 = fromEnum $ B.head bs
	w3 = fromEnum $ B.index bs 1
	w4 = fromEnum $ B.index bs 2
	acc = if chker w2 && chker w3 && chker w4 then
		  (w2 .&. 0x3f) + (w3 .&. 0x3f) + (w4 .&. 0x3f)
	      else -1

chker w = w .&. 0xc0 == 0x80
{-# INLINE chker #-}

