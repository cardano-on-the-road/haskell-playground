module Emurgo1 where

factorial:: Integer -> Integer
factorial i 
    | i == 0 = 1
    | i == 1 = 1
    | otherwise = i * factorial (i - 1) 

factorial':: Integer -> Integer
factorial' 0 = 1
factorial' 1 = 1
factorial' i = i * factorial (i - 1) 

factorial'':: Integer -> Integer
factorial'' i = case i of 
    0 -> 1
    1 -> 1
    i -> i * factorial''( i - 1)

factorial''':: Integer -> Integer
factorial''' i = case (i < 2) of 
    True -> 1
    False -> i * factorial'''(i - 1)
                        

