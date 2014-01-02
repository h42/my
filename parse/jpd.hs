import Parse

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
    print $ runParse (doit) "abc   defg"
    print ""
    print $ runParse (toEol>>toEol>>toEol) "abc   defg\nline 2\r\nline 3\rline4"
    print ""
    print $ runParse (string "that" <|> string "this") "this is the end"
