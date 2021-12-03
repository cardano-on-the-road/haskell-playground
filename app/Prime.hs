module Prime where

isPrime:: Int -> Bool
isPrime i = isPrime' i (div i 2 + 1)

isPrime':: Int -> Int -> Bool
isPrime' 0 _ = False
isPrime' 1 _ = True
isPrime' _ 0 = True
isPrime' _ 1 = True
isPrime' i j = (mod i j > 0) && isPrime' i (j-1)

primeList:: Int -> [Int]
primeList len = let xs = [0 .. len] 
    in 
    [a | a <- xs, isPrime a]

primeList' :: Int -> Int ->[Int]
primeList' low high = [a | a <- [low .. high], isPrime a]

primes :: Int -> Int -> [Int]
primes low high = [p | x <-[1..((high `div` 2) + 1)], p <-[low..high], p `mod` x > 0]

{-
primes :: [Int] -> [Int]
primes xs = primeList''' xs
  where
    primeList''' [] = []
    primeList''' (p:xs) = p : primeList''' [x|x <- xs, x `mod` p > 0]
    -}


    