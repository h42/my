import Parse
import Data.Word

data IP = IP Word8 Word8 Word8 Word8 deriving Show

parserIP = do
    d1 <- digit
    return (d1)

doit = do
    a <- word
    spaces
    b <- word
    toEol
    return (a,b)

doit2 = do
    char 'a'
    --char 'c' <|> char
    y<-anyChar
    --traceShow y anyChar
    spaces
    count 3 anyChar
    return ()

main = do
    print $ runParse (many1 digit) "123 456 def"
    {-
    print $ runParse (doit) "abc   defg"
    print $ runParse doit3 "abc def"
    print $ runParse (toEol>>toEol>>toEol) "abc   defg\nline 2\r\nline 3\rline4"
    print $ runParse (string "that" <|> string "this") "this is the end"
    -}
