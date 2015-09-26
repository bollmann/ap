{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module AVL (Set(..),AVL(..),
            avlEmpty,avlElements,avlMember,avlInsert,avlDelete,
            t1,t2,t3,bad1,bad2,bad3,main,rebalance,height,bf,
            setProperties,prop_empty,prop_elements,prop_insert1,
            prop_insert2,prop_delete1,prop_delete2,prop_delete3,
            avlProperties,prop_bst,prop_ht,prop_balance) where
import Prelude hiding (zipWith,zipWith3)
import Test.QuickCheck hiding (elements)
import Control.Applicative ((<|>))
import Control.Monad

class Set s where
   empty    :: s a
   member   :: Ord a => a -> s a -> Bool
   insert   :: Ord a => a -> s a -> s a
   elements :: s a -> [a]
   delete   :: Ord a => a -> s a -> s a

instance Set AVL where
   empty    = avlEmpty
   member   = avlMember
   insert   = avlInsert
   elements = avlElements
   delete   = avlDelete

-- 1 

prop_empty :: Bool
prop_empty = null $ (elements :: AVL a -> [a]) empty

prop_elements :: AVL Int -> Bool
prop_elements t = all (`member` t) $ elements t

prop_insert1 :: Int -> AVL Int -> Bool
prop_insert1 n t = n `member` (insert n t)

prop_insert2 :: Int -> AVL Int -> Bool
prop_insert2 n t = elements (insert n (insert n t)) == elements (insert n t)

prop_delete1 :: AVL Int -> Bool
prop_delete1 t = foldr delete t (elements t) == empty

prop_delete2 :: AVL Int -> Bool
prop_delete2 = undefined

prop_delete3 :: AVL Int -> Int -> Property
prop_delete3 t n = not (n `member` t) ==> delete n t == t

setProperties :: Property
setProperties = 
  counterexample "empty"   prop_empty    .&&.
  counterexample "elts"    prop_elements .&&. 
  counterexample "insert1" prop_insert1  .&&.
  counterexample "insert2" prop_insert2  .&&.
  counterexample "delete1" prop_delete1  .&&.
  counterexample "delete2" prop_delete2  .&&.
  counterexample "delete3" prop_delete3 

data AVL e = E           -- empty tree
           | N           -- non-empty tree
               Int       -- cached height of the tree
               (AVL e)   -- left subtree
               e         -- value
               (AVL e)   -- right subtree
  deriving Show

-- | Access the height of the tree
height :: AVL e -> Int
height E = 0
height (N h _ _ _) = h

-- | Calculate the balance factor of a node
bf :: AVL e -> Int
bf E = 0
bf (N _ l _ r) = height l - height r

-- | The tree is a binary search tree
prop_bst :: AVL Int -> Bool
prop_bst E = True
prop_bst (N _ l v r) = orderedHere && prop_bst l && prop_bst r
  where orderedHere = all (<v) (elements l) && all (v<) (elements r)

-- | The height at each node is correctly calculated. 
prop_ht :: AVL Int -> Bool
prop_ht E = True
prop_ht (N h l _ r) = heightOkHere && prop_ht l && prop_ht r
  where heightOkHere = h == 1 + max (height l) (height r)

-- | The balance factor at each node is between -1 and +1.  
prop_balance :: AVL Int -> Bool
prop_balance E = True
prop_balance n@(N _ l _ r) =
  (-1 <= bf n && bf n <= 1) && prop_balance l && prop_balance r

avlProperties :: Property
avlProperties = 
  counterexample "bst"     prop_bst .&&.
  counterexample "height"  prop_ht .&&.
  counterexample "balance" prop_balance

instance (Eq a) => Eq (AVL a) where
  t == t' = (elements t) == (elements t')

instance (Ord e, Arbitrary e) => Arbitrary (AVL e) where
  -- arbitrary :: Gen (AVL e)
  arbitrary = liftM (foldr insert empty) $ listOf arbitrary

-- | an empty AVL tree
avlEmpty :: AVL e
avlEmpty = E

-- | list the elements in the tree, in order
avlElements :: AVL e -> [e]
avlElements E = []
avlElements (N _ l v r) = avlElements l ++ [v] ++ avlElements r

-- | Determine if an element is contained within the tree
avlMember :: Ord e => e -> AVL e -> Bool
avlMember _ E = False
avlMember x (N _ l v r)
  | x < v     = avlMember x l
  | x > v     = avlMember x r
  | otherwise = True


t1 :: AVL Int
t1 = N 5 (N 2 (N 1 E 1E) 4 E) 7 (N 2 (N 1 E 8 E) 9 E)

t2 :: AVL Int
t2 = N 3 (N 1 E 3 E) 5 (N 2 E 10 (N 1 E 15 E))
         

t3 :: AVL Int
t3 = undefined

bad1 :: AVL Int
bad1 = undefined

bad2 :: AVL Int
bad2 = undefined

bad3 :: AVL Int
bad3 = undefined


-- | Rotate an AVL tree 
rebalance :: (Ord e) => AVL e -> AVL e
rebalance t =
  case t of
   N _ l@(N _ a x (N _ b y c)) z d | bf t == 2 && bf l == -1  -> makeTree a x b y c z d
   N _ l@(N _ (N _ a x b) y c) z d | bf t == 2 && bf l ==  1  -> makeTree a x b y c z d
   N _ a x r@(N _ (N _ b y c) z d) | bf t == -2 && bf r == 1  -> makeTree a x b y c z d
   N _ a x r@(N _ b y (N _ c z d)) | bf t == -2 && bf r == -1 -> makeTree a x b y c z d
   N _ l v r -> N (1 + max (height l) (height r)) l v r
   E         -> E
  where
    makeTree a x b y c z d =
      let hl = 1 + max (height a) (height b)
          hr = 1 + max (height c) (height d)
          h = 1 + max hl hr
      in N h (N hl a x b) y (N hr c z d)

-- | Insert a new element into a tree, returning a new tree
avlInsert :: (Ord e) => e -> AVL e -> AVL e
avlInsert e E = N 1 E e E
avlInsert e t@(N h l v r)
  | e < v = rebalance $ N h (avlInsert e l) v r
  | e > v = rebalance $ N h l v (avlInsert e r)
  | otherwise = t

-- | Delete the provided element from the tree
avlDelete :: Ord e => e -> AVL e -> AVL e
avlDelete _ E = E
avlDelete e (N h l v r)
  | e < v     = rebalance $ N h (avlDelete e l) v r
  | e > v     = rebalance $ N h l v (avlDelete e r)
  | otherwise =
      case avlMax l of
        Nothing -> r
        Just p  -> rebalance $ N h (p `avlDelete` l) p r

avlMax :: AVL e -> Maybe e
avlMax E = Nothing
avlMax (N _ _ v r) = avlMax r <|> Just v

main :: IO ()
main = return ()

