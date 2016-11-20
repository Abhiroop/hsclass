module Lesson3_abhiroop where

-- (Binary Tree) has a "Node" with (Int) and (Left and Rigth "Binary Tree")
-------              Or it can be ("Empty")

--             BinTree
--       Node |  Int  |
-- BinTree <--         --> BinTree

-- data BinTree = ??

data IntList = Empty | Cons Int IntList deriving Show

absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x = x * x

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)
-- mapIntList addOne exampleList
-- mapIntList abs    exampleList
-- mapIntList square exampleList

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x    = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs

-- filterIntList = ??

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons x xs)
  | p x       = Cons x (filterIntList p xs)
  | otherwise = filterIntList p xs

  
-- How to represent a List which can not be empty ?
-- data NonEmptyList a = ??

-- Exercises from : http://www.cis.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
-- The output of skips
-- is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should
-- contain every second element from the input list. . . and the nth in list in
-- the output should contain every nth element from the input list.
-- For example:
-- skips "ABCD"       == ["ABCD", "BD", "C", "D"] ( 0 1 2 3 4...) (1 3 5....) (2 5 8 11) (3 7 11)

--[0 ...]


-- skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1]          == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips []           == []
-- Note that the output should be the same length as the input.


selectNth :: [a]-> Int -> [a]
selectNth list n = map fst $ filter (\x -> (snd x) `mod` n == 0) $ zip list [1..]

skips :: [a] -> [[a]]
skips list = zipWith selectNth (repeat list) [1..(length list)]

-- Exercise2
-- Local maxima A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it. For
-- example, in the list [2,3,4,1,5], the only local maximum is 4
-- Write a function
-- localMaxima :: [Integer] -> [Integer]
-- which finds all the local maxima in the input list and returns them in
-- order. For example:
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima list = undefined
