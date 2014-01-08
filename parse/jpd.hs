import My.Parse
import Data.Char
import Data.Word

data IP = IP Word8 Word8 Word8 Word8 deriving Show

parseIP :: IO ()
parseIP = do
    let p = runParse parseIP1 "1.1.1.2\n2.3.3.4\n\nuser data"
    print p

parseIP1 = do
    ips <- many1 parseIP2
    u <- parseIP3
    return (ips,u)

parseIP3 = do
    eol
    u <- many anyChar
    return u

parseIP2 :: Parse String (IP)
parseIP2 = do
    d1 <- fmap  read (many1 digit)
    char '.'
    d2 <- fmap  read (many1 digit)
    char '.'
    d3 <- fmap  read (many1 digit)
    char '.'
    d4 <- fmap  read (many1 digit)
    toEol
    return $ (IP d1 d2 d3 d4)

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
    parseIP

    {-
    print $ runParse (many1 digit) "123 456 def"
    print $ runParse (many $ oneIf (isDigit)) "1234 asdf"
    print $ runParse (many $ noneOf "4") "1234 asdf"
    print $ runParse (many $ oneOf "12") "1234 asdf"
    print $ runParse (doit) "abc   defg"
    print $ runParse doit3 "abc def"
    print $ runParse (toEol>>toEol>>toEol) "abc   defg\nline 2\r\nline 3\rline4"
    print $ runParse (string "that" <|> string "this") "this is the end"
    -}
