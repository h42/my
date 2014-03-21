module My.Parse (
    Parse(..)
   ,allP
   ,anyChar
   ,char
   ,count
   ,digit
   ,eol
   ,many
   ,many1
   ,noneOf
   ,oneIf
   ,oneOf
   ,onLeft
   ,toEod
   ,toEol  -- line
   ,space
   ,spaces
   ,string
   ,word
   ,(<|>)
) where

import Data.Char
import Data.List (isPrefixOf)

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

instance Functor (Parse a) where
    fmap f p  = Parse $ \st -> do
        case (runParse p st) of
            Left s -> Left s
            Right (y, ys) -> Right (f y, ys)

char :: Char -> Parse String Char
char c = Parse $ \st -> case st of
    (x:xs) -> if x==c then Right (c,xs)
                      else Left ("char: Bad match on " ++ [c,x])
    []     -> Left "char: EOS"

anyChar :: Parse String Char
anyChar = Parse $ \st -> case st of
    (x:xs) -> Right (x,xs)
    []     -> Left "anyChar: EOS"

digit :: Parse String Char
digit = Parse $ \st ->
        case st of
            (x:xs) -> if isDigit x then Right (x, xs)
                      else Left "digit not found"
            []     -> Left "space: EOS"

--
-- MANY
--
allP :: Parse String a -> Parse String [a]
allP p = Parse $ \st -> allP' p [] st
allP' p as [] = Right (reverse as,[])
allP' p as st = case (runParse p st) of
        Left ls -> Left st
        Right (x,st') -> allP' p (x:as) st'

many :: Parse String a -> Parse String [a]
many p = Parse $ \st -> many' p [] st
many' p as [] = Right (reverse as,[])
many' p as st = case (runParse p st) of
        Left ls -> Right (reverse as,st)
        Right (x,st') -> many' p (x:as) st'

many1 :: Parse String a -> Parse String [a]
many1 p = Parse $ \st -> many1' p [] st
many1' p [] [] = Left "many1: no takers"
many1' p as [] = Right (reverse as,[])
many1' p as st = case (runParse p st) of
        Left ls -> if null as then Left ("many1: " ++ ls)
                   else Right (reverse as, st)
        Right (x,st') -> many1' p (x:as) st'

noneOf :: String -> Parse String Char
noneOf s = Parse $ \st -> case st of
    (x:xs) -> if x `notElem` s then Right (x,xs)
              else Left $ "noneOf: In set"
    []     -> Left "noneOf: EOS"

oneIf :: (Char -> Bool) -> Parse String Char
oneIf f = Parse $ \st -> case st of
    (x:xs) -> if f x then Right (x,xs)
              else Left $ "oneIf: Not found"
    []     -> Left "oneOf: EOS"

oneOf :: String -> Parse String Char
oneOf s = Parse $ \st -> case st of
    (x:xs) -> if x `elem` s then Right (x,xs)
              else Left $ "oneOf: Not in set"
    []     -> Left "oneOf: EOS"

onLeft :: a -> Parse String a -> Parse String a
onLeft a p = Parse $ \st -> case (runParse p st) of
        Right (x,st') -> Right (x,st')
        Left _ -> Right (a,st)
--
--  SPACE
--
space :: Parse String Bool
space = Parse $ \st -> case st of
            (x:xs) -> Right $ (mySpace x, (x:xs))
            []     -> Left "space: EOS"

spaces :: Parse String ()
spaces = Parse $ \xs -> Right ((), dropWhile mySpace xs)

mySpace x = x==' ' || x=='\t'
--
-- STRING
--
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
    _              ->  Left ("eol: Bad match " ++ st )

toEod :: Parse String String
toEod = Parse $ \st ->
    case st of
        xxs@(x:xs) -> Right (xxs,xxs)
        _          -> Left "toEod: Already at eod"

toEol :: Parse String String
toEol = Parse $ \st ->
    let (xs,ys') = break (\x -> x == '\r' || x == '\n') st
        ans = case ys' of
            ('\n':ys)      ->  Right (xs, ys)
            ('\r':'\n':ys) ->  Right (xs, ys)
            ('\r':ys)      ->  Right (xs, ys)
            _              ->  Right (xs, [])
    in ans

