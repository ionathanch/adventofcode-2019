module Day02 where

import Lib
import IntCode (getProgramAfterExec)
import Control.Monad (guard)

addNounVerb :: Int -> Int -> [Int] -> [Int]
addNounVerb n v is = head is : n : v : drop 3 is

part1 :: [Int] -> Int
part1 = head . getProgramAfterExec . addNounVerb 12 2

part2 :: [Int] -> [Int]
part2 input = do
    noun <- [0..99]
    verb <- [0..99]
    let result = head . getProgramAfterExec . addNounVerb noun verb $ input
    guard $ result == 19690720
    return $ 100 * noun + verb

day02 = do
    input <- readAsList <$> readFile "../input/02.txt"
    printPart1 $ part1 input
    printPart2 $ head (part2 input)