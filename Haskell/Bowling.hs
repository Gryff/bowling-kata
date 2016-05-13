module Bowling where 

import Data.Char

scoreGame :: String -> Int 
scoreGame game = foldl addIfInt 0 game

addIfInt :: Int -> Char -> Int
addIfInt total bowl
    | isDigit bowl == True = total + digitToInt bowl
    | otherwise = total 