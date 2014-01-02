import Text.ParserCombinators.Parsec

{-
ps="@11 222 333 444"

symbol :: Parser Char
symbol = oneOf "!@#"

pit xs = case parse ((skipMany space) >> symbol) "11" xs of
    Left err -> "Bad " ++ show err
    Right val -> "Good " ++ [val]

ident :: Parser String

ident = do c <- letter <|> char '_'
	   cs <- many (letter <|> digit <|> char '_')
	   return (c:cs)
	<?> "identifier"
-}

------------------------
-- PARSE 1
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

main = do
    passwd
