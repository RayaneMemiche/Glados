{-
-- EPITECH PROJECT, 2024
-- glados [SSH: pop-os]
-- File description:
-- Utils
-}

module Utils where

import Data.Char (ord, chr)

setAt :: Int -> a -> [a] -> [a]
setAt i x lst = take i lst ++ [x] ++ drop (i + 1) lst

charsToInt :: [Char] -> Int
charsToInt lst | length lst /= 4 = 0
charsToInt ('\0':'\0':'\0':'\0':_) = 0
charsToInt [a, b, c, d] | ord a >= 128 =
    (0xffffffff - (
        ord a * 256 ^ 3 +
        ord b * 256 ^ 2 +
        ord c * 256 +
        ord d - 1
    )) * (-1)
                        | otherwise =
    ord a * 256 ^ 3 + ord b * 256 ^ 2 + ord c * 256 + ord d

charsToUInt :: [Char] -> Int
charsToUInt lst | length lst /= 4 = 0
charsToUInt ('\0':'\0':'\0':'\0':_) = 0
charsToUInt [a, b, c, d] =
    ord a * 256 ^ 3 + ord b * 256 ^ 2 + ord c * 256 + ord d

uintToChars :: Int -> [Int]
uintToChars 0 = [0, 0, 0, 0]
uintToChars int = [a, b, c, d]
    where a = int `div` 256 ^ 3
          b = (int `div` 256 ^ 2) `mod` 256
          c = (int `div` 256) `mod` 256
          d = int `mod` 256
