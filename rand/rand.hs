import My.Rand

main = do
    g<-getStdGen

    -- Lazy versions BLOWS UP ON 10^6 without some IO

    {-
    let (xs,g') = randrs 1 6 (10^6) g -- works for big N
    print $ take 10 xs
    print g
    -}
    let rs = take (10^6) $ randomRs (1,10) g :: [Int]
    print $ last rs

    --print $ take 10 $ map (`mod` 20) (randi g)

