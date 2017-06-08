module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

sampleIntTree :: BinaryTree Int
sampleIntTree =
  Node (Node Leaf 3 Leaf) 5 (Node Leaf 8 Leaf)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node left current right)
  | x == current = Node left x right
  | x < current = Node (insert' x left) current right
  | x > current = Node left current (insert' x right)

{-ex1 map-}
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) =
  Node (mapTree f left) (f x) (mapTree f right)

{-simple assertion-}
mapExpected = Node (Node Leaf 9 Leaf) 25 (Node Leaf 64 Leaf)

mapOk =
  if mapTree (^2) sampleIntTree == mapExpected
     then print "yep ok"
     else error "test failed!"

{-ex2 toList-}
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left current right) =
  [current] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left current right) =
  (inorder left) ++ [current] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left current right) =
  (postorder left) ++ (postorder right) ++ [current] 

ex2SampleTree =
  Node (Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)) 1 (Node Leaf 3 Leaf)

ordersOk = if
              preorder ex2SampleTree == [1,2,4,5,3] &&
              inorder ex2SampleTree == [4,2,5,1,3] &&
              postorder ex2SampleTree == [4,5,2,3,1]

              then print "Awesome work!"
              else error "You have failed me, Execetos"

{-ex3 foldr-}
{-postorder implementation-}
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc Leaf = acc
foldTree f acc (Node left x right) =
  f x rightResult
  where
    leftResult = foldTree f acc left
    rightResult = foldTree f leftResult right

listFromTreeUsingReduce = reverse $ foldTree (:) [] ex2SampleTree

{-ex on page 508-}
unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree f x = case f x of
                 Nothing -> Leaf
                 Just (left, b, right) -> Node (unfoldTree f left) b (unfoldTree f right)

{-ex tree builder-}
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree (\level -> case level of
                                      0 -> Nothing
                                      x -> Just (x - 1, n - x, x - 1)
                         ) n
