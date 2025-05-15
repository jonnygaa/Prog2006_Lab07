module Lib
    ( countScore
    ) where

import Data.List (sort, nub)

-- | Count the total score of the five balls lottery game data.
-- The input data is a text string with many games. Each game is represented as a single line.
-- The first three numbers are the "Winning Numbers", and the next five are the lottery numbers.
-- Each row ends with end-of-line character.
-- Write a function that uses the utility functions `processLine` to calculate the total score of the game.
countScore :: String -> Int
countScore txt =
    sum [processLine x | x <- (lines txt)]

-- | Process a single line of the input data.
-- Each line represents a game, the first three numbers are the "Winning Numbers", 
-- and the next five are the lottery numbers. Calculate the score for the single game.
--
-- >>> processLine "4 21 10 5 4 21 13 11"
-- 5
--
-- >>> processLine "4 21 10 5 4 21 13 11 10"
-- 7
--
-- >>> processLine "4 21 10 5 4 21 13 11 10 10"
-- 11
--
-- >>> processLine "10 21 10 5 8 20 13 11"
-- 0
--
-- >>> processLine "10 21 10 5 4 21 13 11 10"
-- 8 
--
-- >>> processLine "10 10 10 5 4 21 13 11 10 10 10"
-- 56
--
-- >>> processLine "8 14 16 5 8 14 16 14"
-- 9
-- 
-- >>> processLine "8 14 16 5 8 18 16 12"
-- 3
--
-- >>> processLine "35 35 35 1 5 6 35 16"
-- 32
--
-- >>> processLine "35 35 6 1 5 6 35 35"
-- 49
-- 
-- >>> processLine "35 35 35 1 5 6 35 35"
-- 96
-- 
intList :: String -> [Int]
intList a = read <$> (words a)

count :: Int -> [Int] -> Int
count x y = length([a| a <- y, a == x])

dupli :: Int -> [Int] -> Int
dupli x y = 2 ^ (count x y - 1)

points :: Int -> Int
points x = 2 ^ (x `div` 10)

treat :: Int -> [Int] -> Int
treat x y = (count x y) ^ 2 - (count x y) + 1

tot :: [Int] -> [Int] -> Int
tot a b = sum [dupli x a * (points x) * (treat x b) | x <- (nub b), x `elem` a]

{- 
processLine3 :: String -> Int
processLine3 line =
    intList line >>= (\a ->
    take 3 a     >>= (\b ->
    drop 3 a     >>= (\c ->
    return  )))
-}

{-
 processLine :: String -> Int
processLine line =
    intList line >>= \a ->
    take 3 a     >>= \b ->
    drop 3 a     >>= \c ->
    tot b c
-}

processLine :: String -> Int
processLine line =
    let a = intList line
        b = take 3 a
        c = drop 3 a in
    tot b c

