module My.Time (
    utime
    ,ltime
    ,weekDay
) where

import Data.Time

utime = do
    UTCTime zday zdifftime <- getCurrentTime
    return $ convtime zday $ timeToTimeOfDay zdifftime

ltime = do
    ZonedTime (LocalTime zday ztod) timezone  <-  getZonedTime
    return $ convtime zday ztod

convtime :: Day -> TimeOfDay -> (Int,Int,Int,Int,Int,Int,Double)
convtime zday ztod = (fromEnum yr,mon,day,h,m,s,usec) where
    (yr,mon,day) = toGregorian zday
    h = todHour ztod
    m = todMin ztod
    psec = todSec ztod
    s = round psec
    usec = fromIntegral s :: Double

weekDay :: Int -> Int -> Int -> Int
weekDay y m d = dow' where
    (d2,y2) = if m < 3 then (d + y, y - 1) else (d + y - 2,y)
    dow' = (23 * m `quot` 9 + d2 + 4 + y2 `quot` 4 -
	       y2 `quot` 100 + y2 `quot` 400) `mod` 7


{-
main = do
    utime >>= print
    putStrLn "" >> ltime
    ltime >>= print
-}
