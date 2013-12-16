import MY.Time
import Data.Time

main = do
    utime >>= print
    --putStrLn "" >> ltime
    ltime >>= print

    putStrLn ""
    ZonedTime (LocalTime zday ztod) timezone  <-  getZonedTime
    let zday2 = fromGregorian 2028 5 12
    print $ toGregorian zday
    print $ diffDays zday2 zday
