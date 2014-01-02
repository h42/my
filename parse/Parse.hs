module Parse (
    Parse(..)
   ,anyChar
   ,char
   ,count
   ,eol
   ,toEol
   ,space
   ,spaces
   ,string
   ,word
   ,(<|>)
) where

import Data.Char
import Data.List (isPrefixOf)
import Control.Monad.State
import Debug.Trace

newtype Parse s a = Parse { runParse :: s -> Either String (a,s) }

instance Monad (Parse st) where
    return x = Parse $ \st -> Right (x,st)
    (Parse h) >>= f = Parse newf where
        newf st =
            --case (runParse (Parse h) st) of
            case (h st) of
                    Left s -> Left s
                    Right (a, newParse) -> runParse (f a) newParse

infixl 3 <|>

(Parse h) <|> f = Parse newf where
    newf st = case (h st) of
                Left _ -> runParse (f  ) st
                Right (a, newParse) -> Right (a, newParse)

char :: Char -> Parse String Char
char c = Parse $ \st -> case st of
    (x:xs) -> if x==c then Right (c,xs)
                      else Left ("char: Bad match on " ++ [c,x])
    []     -> Left "char: EOS"

anyChar :: Parse String Char
anyChar = Parse $ \st -> case st of
    (x:xs) -> Right (x,xs)
    []     -> Left "anyChar: EOS"

space :: Parse String Bool
space = Parse $ \st -> case st of
            (x:xs) -> Right $ (isSpace x, (x:xs))
            []     -> Left "space: EOS"

spaces :: Parse String ()
spaces = Parse $ \xs -> Right ((), dropWhile isSpace xs)

string :: String -> Parse String String
string s = Parse $ \xs ->
    let ok = isPrefixOf s xs
        l = length s
    in case ok of
        True -> Right (s, drop l xs)
        _    -> Left ("string: failed to find " ++ s)

word :: Parse String String
word = Parse $ \xs ->
    let (xs', ys') = span (\x -> ord x > 32) xs
    in Right (xs', ys')

count ::Int -> Parse String a -> Parse String a
count 1 p = p
count n p = p >> count (n-1) p

eol :: Parse String ()
eol = Parse $ \st -> case st of
    ('\n':xs)      ->  Right ((), xs)
    ('\r':'\n':xs) ->  Right ((), xs)
    ('\r':xs)      ->  Right ((), xs)
    _              ->  Left ("eol: Bad match")

toEol :: Parse String String
toEol = Parse $ \st ->
    let (xs,ys') = break (\x -> x == '\r' || x == '\n') st
        ans = case ys' of
            ('\n':ys)      ->  Right (xs, ys)
            ('\r':'\n':ys) ->  Right (xs, ys)
            ('\r':ys)      ->  Right (xs, ys)
            _              ->  Right (xs, [])
    in ans

