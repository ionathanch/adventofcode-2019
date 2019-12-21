module Main where

import Day02 (day02)

main :: IO ()
main = do
    putStrLn "Which day's solution would you like to execute?"
    day <- readLn
    case day of
        2 -> day02
        n ->
            if n >= 1 && n <= 25
            then putStrLn "Sorry, there's no implementation for that day."
            else putStrLn "That's not even... no!"