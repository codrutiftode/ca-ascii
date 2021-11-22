module Tree (Tree, treeHasNode, treeFromList, treeToList)
where

data Tree a = Leaf
            | Node a (Tree a) (Tree a) 

-- Inserts a node into a tree
insertNode :: Ord a => a -> Tree a -> Tree a
insertNode node = go
    where go Leaf = Node node Leaf Leaf
          go (Node n left right) | node == n = Node n left right
                                 | node < n  = Node n (go left) right
                                 | node > n  = Node n left (go right)


-- Looks up an element in the tree
treeHasNode :: Ord a => a -> Tree a -> Bool
treeHasNode node Leaf = False
treeHasNode node (Node n left right) 
        | node == n = True
        | node < n  = treeHasNode node left
        | node > n  = treeHasNode node right


-- Converts list to tree
treeFromList :: Ord a => [a] -> Tree a
treeFromList = foldr insertNode Leaf 

-- Converts tree to list
treeToList :: Tree a -> [a]
treeToList Leaf = [] 
treeToList (Node n left right) = treeToList left ++ [n] ++ treeToList right 
