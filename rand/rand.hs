import System.Random
import My.Rand

main = do
    g<-getStdGen

    -- Lazy versio BLOWS UP ON 10^6 without some IO
    --let (lazyxs,lazyg') = lazyRandrs 1 6 (10^6) g

    let (xs,g') = randrs 1 6 (10^6) g -- works for big N

    print $ take 10 xs
    print g

