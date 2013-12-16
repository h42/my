import Control.Monad
main = liftM (\s->show ((read s -32) * 5 / 9 :: Double)) getLine >>= putStrLn
