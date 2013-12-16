import Stat

rs :: [Double]
rs = [1,1,2,2,2,3,4,5,5,9,10]

main = do
    print $ mode [1,2,3,4,4,5,5,5,6,7,8,9,0]
    print $ popMean [0..9] (repeat 0.1)
    print $ popVariance [1..5] [0.2,0.3,0.2,0.2,0.1]
    print $ popStdDev [1..5] [0.2,0.3,0.2,0.2,0.1]

    print $ popVariance [0..4] [0.2,0.15,0.25,0.05,0.35]
    {-
    print $ mean rs
    print $ variance rs
    print $ variance1 rs
    let n = fromIntegral (length rs)
    print $ variance rs * n / (n-1)
    let fm = freq [1,2,2,3,3,3,3,3]
    print fm
    print $ pmf 3 fm
    -}
