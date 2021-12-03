module Emurgo2 where

sum':: (Num a) => a -> a -> a
sum' x y = let first = x + 1
               second = y -1
            in
            first + second 


-- Home work mytest

myTake:: Int -> [Int] -> [Int]
myTake 0 xs =  xs
myTake i xs = case i >= length xs of
                    True -> xs
                    False -> myTake' i 0 xs

myTake':: Int -> Int -> [Int] -> [Int]
myTake' i c (x:xs) = case i > c of
                    True -> x: myTake' i (c+1) xs
                    False -> []

myTake''':: Int -> [Int] -> [Int]
myTake''' 0 xs =  xs
myTake''' _ [] = []
myTake''' 1 (x:xs) = [x]
myTake''' i (x:xs) = x: myTake''' (i-1) xs

insertAt:: Int -> Int ->[Int] -> [Int]
insertAt 0 val xs = (val:xs)
insertAt idx val xs = if (idx > length xs) 
   then xs ++ [val] 
   else let (ys, zs) = splitAt idx xs 
      in ys ++ [val] ++ zs

insertAt':: Int -> Int -> [Int] -> [Int]
insertAt 0 val xs = val:xs
insertAt idx val (x:xs) = (idx - 1) 

{- encoding:: [Char] -> [(Char, Int)]
encoding [] = []
encoding (x:y:xs) 
   | x == y = helper (x, 1) (y:xs) 
   | otherwise = [(x, 1)] + (y:xs)
where
   helper:: (Char, Int) -> [Char] -> [(Char, Int)]
   helper (c, i), l 
   —}
   
      