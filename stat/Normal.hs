module Normal (
    normLeft
    ,normRange
    ,normRight
    ,getZ
    ,getZLeft
) where

import Control.Monad

{-main = do
    let z = cumNorm 1
    print $ cumNorm 1
-}

getZ :: Double -> Double -> Double -> Double
getZ u s x = (x - u) / s

getZLeft p = if t'<0 then Nothing
                     else if p >= 0.5 then Just (n'/100)
                     else Just (-n'/100)
  where
    p' = if p >= 0.5 then p else 1-p
    (n',t') = loop 0 (-1) table
    loop n t [] = (n,t)
    loop n t (x:xs) = if x < p' then loop (n+1) x xs
                      else if x==abs p' || t<0 then (n,x) else (n-1,t)

-- probabilty from z1 to z2
normRange z1 z2 = liftM2 (-) (normLeft z2) (normLeft z1)

-- cumulative probabilty of Z > z1
normRight z1 = liftM2 (-) (Just 1) (normLeft z1)

-- cumulative probabilty of Z < z
normLeft z
    | z>3.09 || z< -3.09 = Nothing
    | z>0 = Just (head $ drop (round (z*100)) table )
    | z<0 = Just (1 - (head $ drop (round (-z*100)) table))
    | otherwise = Just 0.5


mkTable = do
    sx <- readFile "Normal.dat"
    let ns = map read (words sx) :: [Double]
        printTable n d = do
            when ((n) `mod` 10 == 0) (putStr "\n    ")
            putStr $ show d ++ if n>1 then ", " else "]\n"
            return (n-1)
    putStr $ " ["
    foldM printTable (length ns) ns

table :: [Double]
table =
   [0.5, 0.504, 0.508, 0.512, 0.516, 0.5199, 0.5239, 0.5279, 0.5319, 0.5359,
    0.5398, 0.5438, 0.5478, 0.5517, 0.5557, 0.5596, 0.5636, 0.5675, 0.5714, 0.5753, 
    0.5793, 0.5832, 0.5871, 0.591, 0.5948, 0.5987, 0.6026, 0.6064, 0.6103, 0.6141, 
    0.6179, 0.6217, 0.6255, 0.6293, 0.6331, 0.6368, 0.6406, 0.6443, 0.648, 0.6517, 
    0.6554, 0.6591, 0.6628, 0.6664, 0.67, 0.6736, 0.6772, 0.6808, 0.6844, 0.6879, 
    0.6915, 0.695, 0.6985, 0.7019, 0.7054, 0.7088, 0.7123, 0.7157, 0.719, 0.7224, 
    0.7257, 0.7291, 0.7324, 0.7357, 0.7389, 0.7422, 0.7454, 0.7486, 0.7517, 0.7549, 
    0.758, 0.7611, 0.7642, 0.7673, 0.7704, 0.7734, 0.7764, 0.7794, 0.7823, 0.7852, 
    0.7881, 0.791, 0.7939, 0.7967, 0.7995, 0.8023, 0.8051, 0.8078, 0.8106, 0.8133, 
    0.8159, 0.8186, 0.8212, 0.8238, 0.8264, 0.8289, 0.8315, 0.834, 0.8365, 0.8389, 
    0.8413, 0.8438, 0.8461, 0.8485, 0.8508, 0.8531, 0.8554, 0.8577, 0.8599, 0.8621, 
    0.8643, 0.8665, 0.8686, 0.8708, 0.8729, 0.8749, 0.877, 0.879, 0.881, 0.883, 
    0.8849, 0.8869, 0.8888, 0.8907, 0.8925, 0.8944, 0.8962, 0.898, 0.8997, 0.9015, 
    0.9032, 0.9049, 0.9066, 0.9082, 0.9099, 0.9115, 0.9131, 0.9147, 0.9162, 0.9177, 
    0.9192, 0.9207, 0.9222, 0.9236, 0.9251, 0.9265, 0.9279, 0.9292, 0.9306, 0.9319, 
    0.9332, 0.9345, 0.9357, 0.937, 0.9382, 0.9394, 0.9406, 0.9418, 0.9429, 0.9441, 
    0.9452, 0.9463, 0.9474, 0.9484, 0.9495, 0.9505, 0.9515, 0.9525, 0.9535, 0.9545, 
    0.9554, 0.9564, 0.9573, 0.9582, 0.9591, 0.9599, 0.9608, 0.9616, 0.9625, 0.9633, 
    0.9641, 0.9649, 0.9656, 0.9664, 0.9671, 0.9678, 0.9686, 0.9693, 0.9699, 0.9706, 
    0.9713, 0.9719, 0.9726, 0.9732, 0.9738, 0.9744, 0.975, 0.9756, 0.9761, 0.9767, 
    0.9772, 0.9778, 0.9783, 0.9788, 0.9793, 0.9798, 0.9803, 0.9808, 0.9812, 0.9817, 
    0.9821, 0.9826, 0.983, 0.9834, 0.9838, 0.9842, 0.9846, 0.985, 0.9854, 0.9857, 
    0.9861, 0.9864, 0.9868, 0.9871, 0.9875, 0.9878, 0.9881, 0.9884, 0.9887, 0.989, 
    0.9893, 0.9896, 0.9898, 0.9901, 0.9904, 0.9906, 0.9909, 0.9911, 0.9913, 0.9916, 
    0.9918, 0.992, 0.9922, 0.9925, 0.9927, 0.9929, 0.9931, 0.9932, 0.9934, 0.9936, 
    0.9938, 0.994, 0.9941, 0.9943, 0.9945, 0.9946, 0.9948, 0.9949, 0.9951, 0.9952, 
    0.9953, 0.9955, 0.9956, 0.9957, 0.9959, 0.996, 0.9961, 0.9962, 0.9963, 0.9964, 
    0.9965, 0.9966, 0.9967, 0.9968, 0.9969, 0.997, 0.9971, 0.9972, 0.9973, 0.9974, 
    0.9974, 0.9975, 0.9976, 0.9977, 0.9977, 0.9978, 0.9979, 0.9979, 0.998, 0.9981, 
    0.9981, 0.9982, 0.9982, 0.9983, 0.9984, 0.9984, 0.9985, 0.9985, 0.9986, 0.9986, 
    0.9987, 0.9987, 0.9987, 0.9988, 0.9988, 0.9989, 0.9989, 0.9989, 0.999, 0.999]
