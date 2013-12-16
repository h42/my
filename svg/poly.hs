import My.Svg

f cx cy r n = do
    svg $ do
        --circle cx cy r [fill "white", stroke "black"]
        path (poly_path cx cy r n) [stroke "red",fill "none"]
        path (poly_rads cx cy r n) [stroke "red",fill "none"]

poly_xy :: Int -> Int -> Int -> Int -> [(Int,Int)]
poly_xy cx cy r n = rs where
    xs = map (\x -> 2 * pi / fromIntegral n * fromIntegral x) [0 .. (n-1)]
    r' = fromIntegral r
    rs = map (\x->(cx + (round $ r'*cos x), cy + (round $ r'*sin x))) xs

poly_rads cx cy r n = ts where
    rs = poly_xy cx cy r n
    fmove = "M" ++ show cx ++ " " ++ show cy
    fline x y = "L" ++ show x ++ " " ++ show y
    showxy (x,y) = [fmove, fline x y]
    ts = (concatMap showxy rs) ++ ["Z"]

poly_path cx cy r n = ts where
    rs = poly_xy cx cy r n
    fmove = "M" ++ show (cx+r) ++ " " ++ show cy
    fline x y = "L" ++ show x ++ " " ++ show y
    showxy (x,y) = [fline x y]
    ts = [fmove] ++ (concatMap showxy rs) ++ ["Z"]

main = do
    let s = render $ f 200 200 100 7
    putStrLn s
    --print (poly_xy 200 200 100)
