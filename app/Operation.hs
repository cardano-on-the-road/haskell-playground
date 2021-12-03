module Operazione where

operazione:: Int -> Char -> Int  -> Int
operazione a op b
    |   op == '+' = a + b
    |   op == '-' = a - b
    |   otherwise = 0