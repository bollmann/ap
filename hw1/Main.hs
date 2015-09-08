{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

{-# LANGUAGE NoImplicitPrelude #-}

-- CIS 552 Homework Assignment #1
-- Dominik Bollmann, Ishan Srivastava, Stuart Wagner
-- Due: Sep, 8th, 2015

module Main where

import Data.List (intersect)
import Data.List (maximumBy)
import Data.Ord
import Prelude hiding (all, reverse, takeWhile, zip, concat, concatMap)
import Test.HUnit

main :: IO ()
main = do 
  _ <- runTestTT $ TestList [ testStyle,
                               testLists,
                               testBowlingKata,
                               testLcs ]
  return ()

--------------------------------------------------------------------------------

testStyle :: Test
testStyle = "testStyle" ~:
  TestList [ tabc , tarithmetic, treverse, tzap ]

-- | Computes a boolean function
abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || z) 
 
tabc :: Test
tabc = "abc" ~: TestList [abc True False True ~?= True, 
                          abc True False False ~?= False,
                          abc False True True ~?= False]


-- | Performs some weird arithmetic over tuples of tuples.
arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic ((a,b),c) ((d,e),f) = (b*f - c*e, c*d - a*f, a*e - b*d)

tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3), 
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]


-- | Reverses a list
reverse :: [a] -> [a]
reverse = reverse' []
  where
    reverse' acc l
      | null l = acc
      | otherwise = reverse' (head l : acc) (tail l)

treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]


-- | Applies the function list point-wise to the argument list.
--   Returns the corresponding return value.
zap :: [a -> b] -> [a] -> [b]
zap (f:fs) (x:xs) = f x : zap fs xs
zap _ _ = []

tzap :: Test
tzap = "zap" ~:
  TestList [ zap [ (+1), \n -> n - 1, (+1) ] ([3, 4, 5] :: [Int]) ~?= [4,3,6],
             zap [ null, not . null ] [ [], "a" ] ~?= [True, True],
             zap [] "a" ~?=  "",
             zap [not] [] ~?= []]

-------------------------------------------------------------------------------- 

testLists :: Test
testLists = "testLists" ~: TestList [tintersperse, tinvert, ttakeWhile, tfind, tall, tmap2, tzip, ttranspose, tconcat]

-- The intersperse function takes an element and a list 
-- and intersperses that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

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
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")] 
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []
--

invert :: [(a,b)] -> [(b,a)]
invert []         = []
invert ((x,y):xs) = (y,x) : invert xs

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
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = []

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
find _ [] = Nothing
find p (x:xs)
  | p x = Just x
  | otherwise = find p xs

tfind :: Test
tfind = "find" ~: TestList
  [ find odd [0,2,3,4] ~?= Just 3
  , find even [1,3,5,7] ~?= Nothing
  , find undefined [] ~?= (Nothing :: Maybe Int)
  ]


-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs

tall :: Test
tall = "all" ~: TestList
  [ all odd [1,2,3] ~?= False
  , all even [2,4,6,8] ~?= True
  , all even [] ~?= True
  ]


-- map2 f xs ys returns the list obtained by applying f to 
-- each pair of corresponding elements of xs and ys. If 
-- one list is longer than the other, then the extra elements 
-- are ignored.
-- i.e. 
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1] 
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the standard library.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys
map2 _ _ _ = []

tmap2 :: Test
tmap2 = "map2" ~: TestList
  [ map2 (*) [1,2,3] [9,9,9] ~?= [9,18,27]
  , map2 (+) [1,2,3] [1,2,3,4,5] ~?= [2,4,6]
  , map2 (+) [1,1,1] [] ~?= []
  ]


-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:  
--    zip [1,2] [True] returns [(1,True)]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []

tzip :: Test
tzip = "zip" ~: TestList
  [ zip [1,2,3] [9,42,21,25] ~?= [(1,9),(2,42),(3,21)]
  , zip [] [1,2,3] ~?= ([] :: [(Int,Int)])
  ]


-- transpose  (WARNING: this one is tricky!)
-- The transpose function transposes the rows and columns of its argument. 
-- If the inner lists are not all the same length, then the extra elements
-- are ignored.
-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]

transpose :: [[a]] -> [[a]]
transpose list
  | any null list = []
  | otherwise =
      foldr buildRow [] list : transpose (foldr deleteRow [] list)
  where
    buildRow cols row = take 1 cols ++ row
    deleteRow row rows = drop 1 row : rows

transpose' :: [[a]] -> [[a]]
transpose' l
  | any null l = []
  | otherwise  = map head l : transpose' (map tail l)

ttranspose :: Test
ttranspose = "transpose" ~: TestList
  [ transpose [[1,2,3],[4,5,6]] ~?= [[1,4],[2,5],[3,6]]
  , transpose [[1,2,3,42],[4,5,6],[7,8,9,10,11]] ~?=
      [[1,4,7],[2,5,8],[3,6,9]]
  , transpose [[1,2,3], []] ~?= []
  , transpose [[1,2], [3,4,5]] ~?= [[1,3],[2,4]]
  ]


-- concat
-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
 
concat :: [[a]] -> [a]
concat = foldr (++) []

tconcat :: Test
tconcat = "concat" ~: TestList
  [ concat [[1,2,3],[4,5,6],[7,8,9]] ~?= [1,2,3,4,5,6,7,8,9]
  , concat [[1,2,3],[],[42,21]] ~?= [1,2,3,42,21]
  ]

-- concatMap
-- Map a function over all the elements of the list and concatenate the results.
-- for example:
--    concatMap (\x -> [x,x+1,x+2]) [1,2,3]  returns [1,2,3,2,3,4,3,4,5]

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f l = concat [ f x | x <- l ]

tconcatMap :: Test
tconcatMap = "concatMap" ~: TestList
  [ concatMap (\x -> [x,x+1,x+2]) [1,2,3] ~?= [1,2,3,2,3,4,3,4,5]
  , concatMap (\x -> [x,x+1,x+2]) [] ~?= []
  , concatMap (\x -> [x,-x]) [1,2] ~?= [1,-1,2,-2]
  ]

--------------------------------------------------------------------------------
-- | Bowling TestKata for calculating the scores for a bowling game

-- | Case0: All Gutter balls 
bowlingTest0 :: ([Int] -> Int) -> Test
bowlingTest0 score = "all gutter balls" ~: 0 ~=? score (replicate 20 0)

score0 :: [Int] -> Int
score0 _ = 0

-- | Case1: All normal scores with no strikes or spares
bowlingTest1 :: ([Int] -> Int) -> Test
bowlingTest1 score = 
   "allOnes" ~: 20 ~=? score (replicate 20 1)

score1 :: [Int] -> Int
score1 = sum

-- | Case2: With 1 spare.
bowlingTest2 :: ([Int] -> Int) -> Test
bowlingTest2 score = "spare test" ~: 18 ~=? score ([3,7,4] ++ replicate 17 0)

score2 :: [Int] -> Int
score2 (x:x':xs)
  | x + x' == 10 = 10 + spare xs
  | otherwise    = x + x' + score2 xs
  where
    spare [] = 0
    spare l@(y:_) = y + score2 l
score2 _ = 0 

score2a :: [Int] -> Int
score2a = score2


-- | Case3: Including Strikes
bowlingTest3 :: ([Int] -> Int) -> Test
bowlingTest3 score = "strike test" ~: (25 + 19 + 9 + 42) ~=? score ([10,10,5,4] ++ replicate 14 3)

score3 :: [Int] -> Int
score3 (x:x':xs)
  | x == 10      = 10 + strike (x':xs)
  | x + x' == 10 = 10 + spare xs
  | otherwise    = x + x' + score3 xs
  where
    strike [] = 0 -- impossible case!
    strike l@[y] = y + score3 l
    strike l@[y,y'] = y + y'  + score3 l
    strike l@(y:y':_) = y + y' + score3 l
    spare [] = 0
    spare l@(y:_) = y + score3 l
score3 _ = 0 

-- | Case5: Combining everything together with spares and strikes.
bowlingTest4 :: ([Int] -> Int) -> Test
bowlingTest4 score = "perfect game" ~: 300 ~=? score (replicate 12 10)

score4 :: [Int] -> Int
score4 = score where
  score (x:y:xs) 
    -- Special Case: Last Strike with 2 additional bonus.
    | x == 10 && length xs == 1 = 10 + strikeBonus (y:xs)    
    -- Strike Case with bonus on next 2 pins.
    | x == 10 && length xs > 1  = 10 + strikeBonus (y:xs) + score (y:xs)
    -- Spare Case with bonus on next pin.
    | x + y == 10               = 10 + spareBonus xs + score xs
    -- Normal Case
    | otherwise                 = x + y + score xs
    where
      spareBonus (a:_)    = a 
      spareBonus _        = 0
      strikeBonus (a:b:_) = a + b
      strikeBonus _       = 0
  score _    = 0

-- |Testcase with strike at the end
bowlingTest5 :: ([ Int ] -> Int) -> Test
bowlingTest5 score = "Complex game" ~: 133 ~=? score [4,2,5,3,2,2,10,10,4,5,7,3,2,4,2,8,10,10,5]

-- |Testcase with spare at the end
bowlingTest6 :: ([ Int ] -> Int) -> Test
bowlingTest6 score = "Complex game2" ~: 132 ~=? score [4,2,10,2,2,10,10,4,5,7,3,2,4,10,4,6,8]

testBowlingKata :: Test
testBowlingKata = TestList (map checkOutput scores) where
  -- the five test cases, in order 
  bowlingTests  = [bowlingTest0, bowlingTest1, bowlingTest2, 
                   bowlingTest3, bowlingTest4]
 
  -- the names of the score functions, the functions themselves, 
  -- and the expected number of passing tests
  scores = zip3 ['0' ..] [score0, score1, score2a, score3, score4] [1..]
 
  -- a way to run a unit test without printing output 
  testSilently = performTest (\ _ _ -> return ()) 
                   (\ _ _ _ -> return) (\ _ _ _ -> return) ()
 
  -- run each bowling test on the given score function, making sure that 
  -- the expected number of tests pass.
  checkOutput (name, score, pass) = " Testing score" ++ [name] ~: do 
    (s0,_) <- testSilently $ (TestList $ bowlingTests `zap` (repeat score))
    assert $ pass @=? cases s0 - (errors s0 + failures s0)

-------------------------------------------------------------------------------- 

-- | Computes all subsequences of the given string.
-- | The behavior of ties is determined by the implementation of `intersect`.
subseqs :: String -> [String]
subseqs []     = [[]]
subseqs (x:xs) = concatMap (\subseq -> [subseq, x:subseq]) (subseqs xs)

-- | Computes the longest common subsequence of the two strings.
lcs :: String -> String -> String 
lcs s1 s2 =
  maximumBy (comparing length) $ subseqs s1 `intersect` subseqs s2

testLcs :: Test
testLcs = "Lcs" ~: TestList
  [ lcs "" "cisab" ~?= ""
  , lcs "Advanced" "Advantaged" ~?= "Advaned"
  , lcs "abcd" "acbd" ~?= "acd"
  , lcs "acabcbc" "abc" ~?= "abc"
  , lcs "hello" "world" ~?= "o"
  , lcs "foo" "bar" ~?= ""
  ]
