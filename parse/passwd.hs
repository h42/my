import Text.ParserCombinators.Parsec

main = passwd

------------------------
-- PASSWD 1
------------------------
passwd = do
    ps <- (readFile "/etc/passwd")
    case parse csv "" ps of
	Left e -> print e
	Right g -> mapM_ print g

csv = endBy line eol
line = sepBy cell (char ':')
cell = many (noneOf ",\n")
eol = char '\n'

{-
passwd = do
    ps <- (readFile "/etc/passwd")
    case parse line "" ps of
	Left e -> print e
	Right g -> mapM_ print g

line :: Parser [(String,String,String)]
line = do
    ls <- many vals
    eof
    return ls

vals = do
    v1 <- val
    char ':'
    v2 <- val
    char ':'
    v3 <- val
    many (noneOf "\n")
    eol
    return (v1,v2,v3)

val = do
    many (noneOf ":")

eol :: Parser Char
eol = char '\n'
-}
