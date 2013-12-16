
main = do
    print $ modexp 2 5 99999
    print $ modexp 2 112 99

modexp _ 0 _ =  1
modexp x y modN =  ans where
    z = modexp x (quot y 2) modN
    ans | even y = rem (z*z) modN
	| otherwise = rem (x*z*z) modN
