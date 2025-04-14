{-
-- EPITECH PROJECT, 2024
-- glados [SSH: pop-os]
-- File description:
-- Primitives
-}

module Primitives where

import Data.Char (ord)
import Data.Bits (xor, shiftR, (.&.), (.|.), shiftL)

type MainStack = [Int]

primitiveAdd :: MainStack -> MainStack
primitiveAdd [] = []
primitiveAdd [a] = [a]
primitiveAdd (b:a:xs) = (a + b) : xs

primitiveSub :: MainStack -> MainStack
primitiveSub [] = []
primitiveSub [a] = [a]
primitiveSub (b:a:xs) = (a - b) : xs

primitiveMul :: MainStack -> MainStack
primitiveMul [] = []
primitiveMul [a] = [a]
primitiveMul (b:a:xs) = (a * b) : xs

primitiveDiv :: MainStack -> MainStack
primitiveDiv [] = []
primitiveDiv [a] = [a]
primitiveDiv (b:a:xs) = (a `div` b) : xs

primitiveMod :: MainStack -> MainStack
primitiveMod [] = []
primitiveMod [a] = [a]
primitiveMod (b:a:xs) = (a `mod` b) : xs

primitiveEq :: MainStack -> MainStack
primitiveEq [] = []
primitiveEq [a] = [a]
primitiveEq (b:a:xs) = (if a == b then 1 else 0) : xs

primitiveNeq :: MainStack -> MainStack
primitiveNeq [] = []
primitiveNeq [a] = [a]
primitiveNeq (b:a:xs) = (if a /= b then 1 else 0) : xs

primitiveLt :: MainStack -> MainStack
primitiveLt [] = []
primitiveLt [a] = [a]
primitiveLt (b:a:xs) = (if a < b then 1 else 0) : xs

primitiveGt :: MainStack -> MainStack
primitiveGt [] = []
primitiveGt [a] = [a]
primitiveGt (b:a:xs) = (if a > b then 1 else 0) : xs

primitiveLte :: MainStack -> MainStack
primitiveLte [] = []
primitiveLte [a] = [a]
primitiveLte (b:a:xs) = (if a <= b then 1 else 0) : xs

primitiveGte :: MainStack -> MainStack
primitiveGte [] = []
primitiveGte [a] = [a]
primitiveGte (b:a:xs) = (if a >= b then 1 else 0) : xs

primitiveDisp :: MainStack -> MainStack
primitiveDisp [] = []
primitiveDisp (a:xs) = show a *> (a : xs)

primitiveAnd :: MainStack -> MainStack
primitiveAnd [] = []
primitiveAnd [a] = [a]
primitiveAnd (b:a:xs) = (a .&. b) : xs

primitiveOr :: MainStack -> MainStack
primitiveOr [] = []
primitiveOr [a] = [a]
primitiveOr (b:a:xs) = (a .|. b) : xs

primitiveXor :: MainStack -> MainStack
primitiveXor [] = []
primitiveXor [a] = [a]
primitiveXor (b:a:xs) = (a `xor` b) : xs

primitiveRshift :: MainStack -> MainStack
primitiveRshift [] = []
primitiveRshift [a] = [a]
primitiveRshift (b:a:xs) = (a `shiftR` b) : xs

primitiveLshift :: MainStack -> MainStack
primitiveLshift [] = []
primitiveLshift [a] = [a]
primitiveLshift (b:a:xs) = (a `shiftL` b) : xs

primitiveDup :: MainStack -> MainStack
primitiveDup [] = []
primitiveDup (a:xs) = a : a : xs

primitiveDrop :: MainStack -> MainStack
primitiveDrop [] = []
primitiveDrop stack = drop 1 stack
