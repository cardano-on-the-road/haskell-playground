module Lista where

data Lista = Node Int Lista | EmptyNode 
    deriving Show

singleNode:: Int -> Lista
singleNode i = Node i EmptyNode 

appendValue:: Int -> Lista -> Lista
appendValue value EmptyNode = singleNode value 
appendValue value (Node i l) = Node i (appendValue value l) 

visitList :: Lista -> IO()
visitList EmptyNode = print "END"
visitList (Node i l) = do 
    print i
    visitList l