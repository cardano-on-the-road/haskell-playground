---------------------------------------------------------
-- HOMEWORK
import Data.String

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type Timestamp = Int

data LogMessage = LogMessage MessageType Timestamp String
                | Unknown String
  deriving (Show, Eq)

testString :: String
testString = unlines [
  "I 100 just an info",
  "W 20 just a warning",
  "E 45 150 an error!",
  "E 60 10 another error!",
  "I 200 wow what an info",
  "blah blah blah",
  "E 55 10 yet another error",
  "I 5 just another info",
  "E 45 1350 an error 1350!",
  "E 45 45 an error!",
  "E 45 75 an error 75",
  "E 45 51 an error 51"
   ]

-- Fill these in by hand.
testLogMessages :: [LogMessage]
testLogMessages = undefined

-- parse an entire log file line by line to a list of log messages.
-- hint: Use the lines function and create helper function to parse a single line.
-- also look at the functions words and unwords.
-- search on hoogle.haskell.org
-- Use read to convert a string to an Int
-- Doesn't have to be perfect, it's fine to make assumptions
parseLog :: String -> [LogMessage]
parseLog message = helper $ lines message

    where
        helper:: [String] -> [LogMessage]
        helper [] = []
        helper (x:xs) = [parse $ words(x)] ++ helper (xs)  

        parse:: [String] -> LogMessage
        parse (x:y:z:xs) 
            | (x == "I") = LogMessage Info (read y) (unwords (z:xs))
            | (x == "W") = LogMessage Warning (read y) (unwords (z:xs))
            | (x == "E" ) = LogMessage (Error (read y)) (read z) (unwords xs)
            | otherwise = Unknown $ unwords (x:y:z:xs)



-- to this parseLog, parseLog testString == testLogMessages

-- Assume that MessageTree is always binary search tree.
-- meaning all values on left will have timestamp less than the current node's timestamp
-- and all the values on the right will have timestamp greater than the current.
-- Assume upto 1 log message at a given timestamp.
data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
                 deriving Show

-- insert inserts a logMessage into a tree and returns the new tree
-- if the log message is of unknown type, return the tree as it is.
-- While inserting, make sure that the new tree is also a binary search tree.
-- Read more about binary search tree on wikipedia or the web.

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown m) tree = tree  
insert (LogMessage mt1 tp1 ms1) Leaf = Node Leaf (LogMessage mt1 tp1 ms1) Leaf
insert (LogMessage mt1 tp1 ms1) (Node left (LogMessage mt2 tp2 ms2) right) = if tp1 > tp2 
    then (Node left (LogMessage mt2 tp2 ms2) (insert (LogMessage mt1 tp1 ms1) right))  
    else (Node (insert (LogMessage mt1 tp1 ms1) left) (LogMessage mt2 tp2 ms2) right)

-- returns all the logs in increasing timestamp order
-- Hint: Notice that the tree is a binary search tree, use the properties you know about them!
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log right) = inOrder left ++ [log] ++ inOrder right

-- prints the String from all Log messages that are of type Error and have severity > 50 (in increasing order of timestamp)
-- Hint: This will use insert and inOrder
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong (x:xs) = let 
                            t = buildTree (x:xs) Leaf
                            ol = inOrder t
                            ris = filter filterF ol
                        in 
                            lines $ show ris

    where
        buildTree:: [LogMessage] -> MessageTree -> MessageTree
        buildTree [] tree = tree
        buildTree (x:xs) tree =  let t = insert x tree
                                    in
                                    buildTree xs t
        
        filterF:: LogMessage -> Bool
        filterF (Unknown e) = False
        filterF (LogMessage Info _ _) = False
        filterF (LogMessage Warning _ _) = False
        filterF (LogMessage (Error e) tp s) 
            | tp > 50 = True
            | otherwise = False