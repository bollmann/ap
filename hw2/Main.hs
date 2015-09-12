{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-} 

module Main where

import Prelude hiding (takeWhile, all)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import Test.HUnit      -- unit test support

import XMLTypes        -- support file for XML problem (provided)
import Play            -- support file for XML problem (provided)

doTests :: IO ()
doTests = do 
  _ <- runTestTT $ TestList [ testFoldr, testTree, testXML ]
  return ()

main :: IO ()
main = do 
       doTests
       return ()

----------------------------------------------------------------------

testFoldr :: Test
testFoldr = TestList [tintersperse, tinvert, ttakeWhile, tfind, tall]

-- The intersperse function takes an element and a list 
-- and `intersperses' that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse :: a -> [a] -> [a]
intersperse sep = init . foldr (\x xs -> x:sep:xs) []

tintersperse :: Test
tintersperse = "intersperse" ~: TestList
  [ intersperse ',' "abcde" ~?= "a,b,c,d,e"
  , intersperse '-' "Hello World" ~?= "H-e-l-l-o- -W-o-r-l-d"
  , intersperse 1 [9, 9, 9] ~?= [9, 1, 9, 1, 9]
  , intersperse 42 [] ~?= []
  , intersperse "BAR" ["FOO"] ~?= ["FOO"]
  ]

-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 

invert :: [(a,b)] -> [(b,a)]
invert = map swap

tinvert :: Test
tinvert = "invert" ~: TestList
  [ invert [("a",1), ("a",2)] ~?= [(1,"a"), (2,"a")]
  , invert [] ~?= ([] :: [(Int, Char)])
  ] 

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr go []
  where
    go x acc
      | p x       = x:acc
      | otherwise = acc

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: TestList
  [ takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= ([1,2] :: [Int])
  , takeWhile (< 9) [1,2,3] ~?= ([1,2,3] :: [Int])
  , takeWhile (< 0) [1,2,3] ~?= ([] :: [Int])
  ]
 
-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

find :: (a -> Bool) -> [a] -> Maybe a
find p = foldr go Nothing
  where
    go x acc
      | p x       = Just x
      | otherwise = acc

tfind :: Test
tfind = "find" ~: TestList
  [ find odd [0,2,3,4] ~?= Just 3
  , find even [1,3,5,7] ~?= Nothing
  , find undefined [] ~?= (Nothing :: Maybe Int)
  ] 

-- all pred lst returns False if any element of lst 
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all  :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x r -> p x && r) True

tall :: Test
tall = "all" ~: TestList
  [ all odd [1,2,3] ~?= False
  , all even [2,4,6,8] ~?= True
  , all even [] ~?= True
  ]

----------------------------------------------------------------------

testTree :: Test
testTree = TestList [ tinvertTree, ttakeWhileTree, tallTree, tmap2Tree, tzipTree ]

-- | a basic tree data structure
data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Show, Eq)

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree e _ Leaf             = e
foldTree e n (Branch a n1 n2) = n a (foldTree e n n1) (foldTree e n n2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf (\x t1 t2 -> Branch (f x) t1 t2)

-- The invertTree function takes a tree of pairs and returns a new tree 
-- with each pair reversed.  For example:
--     invertTree (Branch ("a",1) Leaf Leaf) returns Branch (1,"a") Leaf Leaf

invertTree :: Tree (a,b) -> Tree (b,a)
invertTree = mapTree swap

tinvertTree :: Test
tinvertTree = "invertTree" ~: TestList
  [ invertTree (Branch ("a", 1) Leaf Leaf) ~?= (Branch (1, "a") Leaf Leaf)
  , invertTree
      (Branch ("a", 9)
       (Branch ("b", 42) Leaf
        (Branch ("c", 21) Leaf Leaf))
       Leaf)
    ~?=
      (Branch (9, "a")
       (Branch (42, "b") Leaf
        (Branch (21, "c") Leaf Leaf))
       Leaf)
  , invertTree Leaf ~?= (Leaf :: Tree (Char, Char))
  ]
 

-- takeWhileTree, applied to a predicate p and a tree t, 
-- returns the largest prefix tree of t  (possibly empty) 
-- where all elements satisfy p. 
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Leaf Leaf) (Branch 3 Leaf Leaf)

--     takeWhileTree (< 3) tree1 returns Branch 1 (Branch 2 Leaf Leaf) Leaf
--     takeWhileTree (< 9) tree1 returns tree1
--     takeWhileTree (< 0) tree1 returns Leaf

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree p = foldTree Leaf go
  where
    go v lt rt
      | p v       = Branch v lt rt
      | otherwise = Leaf
                    
ttakeWhileTree :: Test
ttakeWhileTree = "takeWhileTree" ~: TestList
  [ takeWhileTree (< 3) tree1 ~?= Branch 1 (Branch 2 Leaf Leaf) Leaf
  , takeWhileTree (< 9) tree1 ~?= tree1
  , takeWhileTree (< 0) tree1 ~?= Leaf
  ]

-- allTree pred tree returns False if any element of tree 
-- fails to satisfy pred and True otherwise.
-- for example:
--    allTree odd tree1 returns False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree p = foldTree True (\x lt rt -> x && lt && rt) . mapTree p

tallTree :: Test
tallTree = "allTree" ~: TestList
  [ allTree odd tree1 ~?= False
  , allTree (`elem` [1,2,3]) tree1 ~?= True
  ]

-- WARNING: This one is a bit tricky!  (Hint: the value
-- *returned* by foldTree can itself be a function.)

-- map2Tree f xs ys returns the tree obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one branch is longer than the other, then the extra elements 
-- are ignored.
-- for example:
--    map2Tree (+) (Branch 1 Leaf (Branch 2 Leaf Leaf)) (Branch 3 Leaf Leaf)
--        should return (Branch 4 Leaf Leaf)

--foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
--foldTree e _ Leaf             = e
--foldTree e f (Branch a n1 n2) = f a (foldTree e f n1) (foldTree e f n2)
--
--mapTree :: (a -> b) -> Tree a -> Tree b
--mapTree f = foldTree Leaf (\x t1 t2 -> Branch (f x) t1 t2)

map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree f (Branch x l1 r1) (Branch y l2 r2) =
  Branch (f x y) (map2Tree f l1 l2) (map2Tree f r1 r2)
map2Tree _ _ _ = Leaf

tmap2Tree :: Test
tmap2Tree = "map2Tree" ~: TestList
  [ map2Tree (+) (Branch 1 Leaf (Branch 2 Leaf Leaf)) (Branch 3 Leaf Leaf)
    ~?= (Branch 4 Leaf Leaf) ]

-- zipTree takes two trees and returns a tree of corresponding pairs. If
-- one input branch is smaller, excess elements of the longer branch are
-- discarded.
-- for example:  
--    zipTree (Branch 1 (Branch 2 Leaf Leaf) Leaf) (Branch True Leaf Leaf) returns 
--            (Branch (1,True) Leaf Leaf)

-- To use foldTree, you'll need to think about this one in
-- the same way as part (d).

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree = map2Tree (,)

tzipTree :: Test
tzipTree = "zipTree" ~: TestList
  [ zipTree (Branch 1 (Branch 2 Leaf Leaf) Leaf) (Branch True Leaf Leaf)
    ~?= (Branch (1,True) Leaf Leaf) ]

----------------------------------------------------------------------

type XMLPath = String

splitOn :: Char -> String -> [String]
splitOn sep = uncurry (:) . foldr go ([],[])
  where
    go chr (split, splits)
      | chr /= sep = (chr:split, splits)
      | otherwise  = ([], split:splits)

getElement :: XMLPath -> SimpleXML -> Maybe [SimpleXML]
getElement path root = getElem (splitOn '/' path) root
  where
    getElem _ (PCDATA _) = Nothing
    getElem [] xml = Just [xml]
    getElem (p:ps) (Element tag children)
      | p == tag  = foldr justs Nothing $ map (getElem ps) children
      | otherwise = Nothing
    justs Nothing accu       = accu
    justs e@(Just _) Nothing = e
    justs (Just x) (Just xs) = Just $ x ++ xs

--removeElement :: XMLPath -> SimpleXML -> SimpleXML
--removeElement path root
--  | tag root == path = error "removeElement: cannot remove root element"
--  | otherwise        = head $ remove (splitOn '/' path) root
--  where
--    remove _ xml@(PCDATA _) = [xml]
--    remove [] xml@(Element _ _) = []
--    remove (p:ps) xml@(Element t cs)
--      | p /= t = [xml]
--      | p == t = [Element t (concatMap (remove ps) cs)]

-- | Substitutes tag names in given 'SimpleXML'.
substitute :: [(XMLPath, String)] -> SimpleXML -> SimpleXML
substitute substs = subst ""
  where
    subst _ xml@(PCDATA _) = xml
    subst context (Element name children) =
      Element newName (map (subst newContext) children)
      where
        newContext | null context = name
                   | otherwise    = context ++ "/" ++ name
        newName = fromMaybe name (lookup newContext substs)

-- | Prunes an XML structure top-down omitting the nodes in 'exceptions'
flattenBut :: [ElementName] -> SimpleXML -> [SimpleXML]
flattenBut _ xml@(PCDATA _) = [xml]
flattenBut exceptions xml@(Element tag cs)
  | tag `elem` exceptions = [xml]
  | otherwise = concatMap (flattenBut exceptions) cs

formatPlay :: SimpleXML -> SimpleXML
formatPlay xml =
  Element "html" $ [ restructure . substitute translations $ xml ]
  where
    putBreak xml@(PCDATA _) = [xml, br]
    putBreak xml@(Element t _)
      | t == "b"  = [xml, br]
      | otherwise = [xml]
    restructure (Element "body" (title:artists:acts))
      = Element "body" $ [title, subtitle] ++
          concatMap putBreak
            (flattenBut [] artists ++
             concatMap (flattenBut [ "h1", "h2", "h3", "b" ]) acts)
    restructure _ = error "formatPlay: input XML format is invalid"
    translations =
      [ ("PLAY", "body")
      , ("PLAY/TITLE", "h1")
      , ("PLAY/ACT/TITLE", "h2")
      , ("PLAY/ACT/SCENE/TITLE", "h3")
      , ("PLAY/ACT/SCENE/SPEECH/SPEAKER", "b")
      ]
    subtitle = Element "h2" [PCDATA "Dramatis Personae"]
    br = Element "br" []

firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds) 
    | c==d = firstDiff cs ds 
    | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)

-- | Test the two files character by character, to determine whether
-- they match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do 
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> return ()
    Just (cs,ds) -> assertFailure msg where
      msg  = "Results differ: '" ++ take 20 cs ++ 
            "' vs '" ++ take 20 ds

testXML :: Test
testXML = TestCase $ do 
  writeFile "dream.html" (xml2string (formatPlay play))
  testResults "dream.html" "sample.html"

