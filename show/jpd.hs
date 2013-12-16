import My.Show

main = do
    let n=143
    print $ (flip ishow) n 10

    print $ fshow (10,3) 31.1234
    print $ fshow (10,5) (-0.0023)
    print $ fshow (10,0) (-0.0023)
    print $ fshow (10,0) (-3.60023)


