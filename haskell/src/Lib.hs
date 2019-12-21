module Lib
    ( readAsList
    , printPart1
    , printPart2
    , (%), (//)
    ) where

-- Read a sequence of comma-separated values as a homogenous list
readAsList :: Read a => String -> [a]
readAsList = read . ("[" ++) . (++ "]")

printPart1 :: Show a => a -> IO ()
printPart1 a = do
    putStr "Part 1: "
    print a

printPart2 :: Show a => a -> IO ()
printPart2 a = do
    putStr "Part 1: "
    print a

(%) :: Integral a => a -> a -> a
(%) = mod

(//) :: Integral a => a -> a -> a
(//) = div
