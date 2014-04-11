{-# LANGUAGE TemplateHaskell #-}

import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers

main = $(quickCheckAll)


-- 1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (x:xs) = myLast xs

prop_myLast xs = length xs > 0 ==>
  fromJust (myLast xs) == last xs


-- 2
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [x] = Nothing
myButLast [x,_] = Just x
myButLast (x:ys) = myButLast ys

prop_myButLast (NonEmpty xs) = length xs > 1 ==>
  xs !! (length xs - 2) == fromJust (myButLast xs)


-- 3
elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:xs) i
  | i > length (x:xs) = Nothing
  | i < 1 = Nothing
  | i == 1 = Just x
  | otherwise = elementAt xs (i - 1)

prop_elementAt (NonEmpty xs) (Positive i) = i <= length xs ==>
  xs !! (i - 1) == fromJust (elementAt xs i)

prop_elementAtOutOfRange xs i = i > length xs ==>
  elementAt xs i == Nothing

prop_elementAtTooSmall xs i = i < 1 ==>
  elementAt xs i == Nothing


-- 4
myLength :: [a] -> Int
myLength xs = foldl acc 0 xs
  where acc i _ = i + 1

prop_myLength xs = length xs == myLength xs


-- 5
myReverse :: [a] -> [a]
myReverse xs = foldl acc [] xs
  where acc xs x = x:xs

prop_myReverse xs = reverse xs == myReverse xs


-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

prop_isPalindrome xs = reverse xs == xs ==>
  isPalindrome xs == True

prop_isPalindromeFalse xs = reverse xs /= xs ==>
  isPalindrome xs == False


-- 7
data NestedList a = Elem a | List [NestedList a]
  deriving (Show)

myFlatten :: NestedList a -> [a]
myFlatten (List []) = []
myFlatten (Elem x) = [x]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)


-- 8
myCompress :: Eq a => [a] -> [a]
myCompress [] = []
myCompress (x:xs) = foldl acc [x] xs
  where acc ys y = if y /= last ys then ys ++ [y] else ys


-- 9
pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = foldl acc [[x]] xs
  where acc ack x = if x == (last $ last ack)
                      then init ack ++ [x:(last ack)]
                      else ack ++ [[x]]

-- 10
encode' :: Eq a => [a] -> [(Int,a)]
encode' xs = map (\x -> ((length x), (head x))) $ pack' xs

decode' :: [(Int,a)] -> [a]
decode' [] = []
decode' ((i,a):xs) = (map (\_ -> a) [1..i]) ++ (decode' xs)

prop_encode xs = (decode' $ encode' xs) == xs
