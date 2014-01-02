import Data.Char
import Data.List

data Lex = LexInt Int
	 | LexReal  Double
	 | LexStr   String
	 | LexChars String -- ++, ||, && etc
	 | LexChar  Char
	 | LexEol
	 | LexEof
	 | LexErr String
    deriving Show

getToks :: String -> [String] -> [Lex]
getToks xs optab = reverse $ gettoks xs optab []

gettoks xs optab ls = case lx of
	LexEof   -> lx:ls
	LexErr _ -> lx:ls
	_        -> gettoks xs' optab (lx:ls)
    where (xs', lx) = getTok xs optab

getTok :: String -> [String] -> (String,Lex)
getTok [] _ = ([],LexEof)
getTok xxs@(x:xs) optab
    | x == '\n' = (xs,LexEol)
    | x=='"' = qstring xs []
    | isAlpha x = getanum xs [x]
    | isDigit x = getnum xs [x]
    | isSpace x = getTok xs optab
    | otherwise = getSpecial xxs optab

getSpecial xxs@(x:xs) [] = (xs,LexChar x)
getSpecial xs (o:optab)
    | isPrefixOf o xs = (drop (length o) xs, LexChars o)
    | otherwise = getSpecial xs optab

getnum :: String -> String -> (String,Lex)
getnum [] y = ([],LexInt (read (reverse y) :: Int))
getnum xxs@(x:xs) ys
    | isDigit x = getnum xs (x:ys)
    | x=='.' = getrnum xs (x:ys)
    | otherwise = (xxs, LexInt (read (reverse ys) :: Int))

getrnum [] ys = ([], LexReal (read (reverse ys) :: Double))
getrnum xxs@(x:xs) ys
    | isDigit x = getrnum xs (x:ys)
    | otherwise = (xxs, LexReal (read (reverse ys) :: Double))

getanum [] ys = ([],LexStr $ reverse ys)
getanum xxs@(x:xs) ys
    | isAlphaNum x = getanum xs (x:ys)
    | otherwise = (xxs,LexStr (reverse ys))

qstring [] _ = ([],LexErr "Missing quote")
qstring ('\n' : xs) _ = (xs,LexErr "Missing quote")
qstring ('"':xs) ys = (xs,LexStr (reverse ys))
qstring (x:xs) ys = qstring xs (x:ys)

main = do
    let sx = "a =  123+456.7*0 ++ abc && 123"
	ls = getToks sx ["++","&&"]
    putStrLn sx
    mapM_ print ls
