gcd' a 0 = 1
gcd' a b = gcd' b (rem a b)

xgcd a 0 = (1,0,a)
xgcd a b = (y,x-quot a b * y,d) where
    (x,y,d) = xgcd b (rem a b)

main = do
    let (a,b) = (100,42)
    print $ gcd a b
    print ""
    let (x,y,d) = xgcd a b
    print $ xgcd a b
    print $ a*x + b*y
