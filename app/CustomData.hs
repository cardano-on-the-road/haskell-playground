module CustomData where
import Data.Aeson (FromJSON, ToJSON)

data Shape = Rectangle Float Float | Triangle Float Float
    deriving Show

area:: Shape -> Float
area (Rectangle a h) = a * h
area (Triangle a h) = a * h / 2

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    } deriving (Show)

