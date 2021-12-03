module MyMaybe where

-- Data
data MyMaybe a = MyJust a | MyNothing
    deriving (Show)

-- Extend TypeClass
instance (Eq m) => Eq (MyMaybe m) where 
    MyJust x == MyJust y = x == y
    MyNothing == MyNothing = False
    _ == _ = False

-- Functor
instance Functor MyMaybe where
    fmap f  (MyJust x) = MyJust (f x)
    fmap f (MyNothing) = MyNothing

-- Applicative
instance Applicative MyMaybe where
    pure x = MyJust x
    MyNothing <*> _ = MyNothing
    (MyJust f) <*> something = fmap f something


-- Monoid

-- Monad
instance Monad MyMaybe where
    return x = MyJust x
    MyNothing >>= f = MyNothing
    MyJust x >>= f = f x
    --MyJust x >> f 

sumMyJust:: (Num a) => MyMaybe a -> MyMaybe a -> MyMaybe a
sumMyJust a b = do 
    x <- a
    y <- b 
    return (x + y)
