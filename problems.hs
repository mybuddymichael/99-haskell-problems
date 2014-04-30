{-# LANGUAGE TemplateHaskell #-}

import Data.List
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
myFlatten (Elem x) = [x]
myFlatten (List xs) = foldl f [] xs
  where f xs x = xs ++ myFlatten x


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

prop_pack xs = (concat $ pack' xs) == xs


-- 10
encode' :: Eq a => [a] -> [(Int,a)]
encode' xs = map (\x -> ((length x), (head x))) $ pack' xs

decode' :: [(Int,a)] -> [a]
decode' xs = foldl f [] xs
  where f xs (i,x) = xs ++ (map (\_ -> x) [1..i])

prop_encode xs = (decode' $ encode' xs) == xs


-- 11
data EncodedElem a = Single a | Multiple Int a
  deriving (Show)

encode'' :: Eq a => [a] -> [EncodedElem a]
encode'' xs = map (\(i,x) -> if i == 1 then Single x else Multiple i x)
    $ encode' xs


-- 12
decode'' :: [EncodedElem a] -> [a]
decode'' xs = foldl' f [] xs
  where f xs (Single x) = xs ++ [x]
        f xs (Multiple i x) = xs ++ (map (\_ -> x) [1..i])

prop_encode2_and_decode xs = (decode'' $ encode'' xs) == xs


-- 13
-- encodeDirect :: Eq a => [a] -> [EncodedElem a]
