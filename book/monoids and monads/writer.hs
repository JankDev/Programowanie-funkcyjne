import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = (return 3) >>= (\x -> logNumber x >>= (\y -> logNumber 5 >>= (\z -> return (y*z))))