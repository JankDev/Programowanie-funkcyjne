fibS :: Integer -> (Integer, Integer)
fibS 0 = (0, 1)
fibS n = (y, x + y)
      where (x, y) = fibS (n-1)

fib2 :: Integer -> Integer
fib2 = fst . fibS

main::IO()
main = do
      putStrLn "Podaj n:" 
      liczba <- getLine
      let n = read liczba :: Integer
      print (fib2 n)
