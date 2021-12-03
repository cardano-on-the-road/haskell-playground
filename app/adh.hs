data IntList = Empty -- equivalent to []
             | Cons Int IntList -- equivalent to (:)
  deriving Show

-- [] and : are also labels (and functions) just like any constructor.

list0 :: IntList
list0 = Empty
list1 :: IntList
list1 = Cons 1 list0 -- Cons 1 Empty
list2 :: IntList
list2 = Cons 2 list1 -- Cons 2 (Cons 1 Empty)

lengthIntList :: IntList -> Int
lengthIntList Empty = 0
lengthIntList (Cons x xs) = 1 + lengthIntList xs

-- Exercise: define a function to product all the elements of an IntList
prodIntList :: IntList -> Int
prodIntList = undefined

data Tree = Leaf
          | Node Tree Int Tree
  deriving Show

dummyTree :: Tree
dummyTree = Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 (Node Leaf 42 Leaf))

existsInTree :: Int -> Tree -> Bool
existsInTree x Leaf = False
existsInTree x (Node left y right) = x == y || existsInTree x left || existsInTree x right

-- EXERCISE: Find the largest number in a tree with only positive integers (1,2,...)
-- If the tree is a leaf, return 0.
largestInTree :: Tree -> Int
largestInTree Leaf = 0
largestInTree (Node left x right) = max x $ max (largestInTree left) (largestInTree right)


-- EXERCISE: multiply all numbers in a tree
prodTree :: Tree -> Int
prodTree Leaf = 1
prodTree (Node left x right) = x * prodTree left * prodTree right 