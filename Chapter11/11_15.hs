data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f  (Node leftNode a rightNode) = Node (mapTree f leftNode) (f a) (mapTree f rightNode)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

-- filterTree :: (a -> Bool) -> BinaryTree a -> BinaryTree a
-- filterTree _ Leaf = Leaf
-- filterTree f (Node leftNode a rightNode)
--   let leftNodeFiltered = filterTree f leftNode
--       rightNodeFiltere = filterTree f rightNode
--   in
--     | f a = (filterTree f leftNode) a (filterTree f rightNode)
--     | leftNode /= Leaf = filterTree (rotateLeft leftNode rightNode)
--     | rightNode /= Leaf = filterTree (rotateRight leftNode rightNode)

-- rotateLeft :: BinaryTree a -> BinaryTree a -> BinaryTree a
-- rotateLeft (Node l' a r') r = Node

-- testTree :: BinaryTree Integer
-- testTree =
--   Node
--   (Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf))
--   0
--   (Node (Node Leaf 5 Leaf) 2 (Node Leaf 6 Leaf))

filterExpected = Node (Node Leaf 1 Leaf) 0 (Node Leaf 2 Leaf)

-- filterOkay =
--   if (filterTree (<3) testTree == filterExpected)
--     then print "yup okay!"
--     else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: Ord a => BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =  (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b ->  b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f acc left) (foldTree f acc right )
