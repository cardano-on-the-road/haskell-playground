{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where
-- import data.List

import Operazione
import Prime

data Person = Person {firstName::String
    , lastName::String
    , age :: Int} deriving (Show)

data Employee = Employee {employeeId:: Int
    , details::Person
} deriving (Show)

operazioneLista:: [a] -> [a]
operazioneLista (xs:s) = s

main :: IO ()
main = do
    print "Prime test ten and seven"
    print (isPrime 10)
    print (isPrime 7)

