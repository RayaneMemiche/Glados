{-
-- EPITECH PROJECT, 2023
-- VM.hs
-- File description:
-- glados
-}

module VM where

import Data.Char ( ord )
import Data.List
import Data.Bits

import Utils (setAt, charsToInt, charsToUInt)
import Primitives (primitiveAdd, primitiveSub, primitiveMul, primitiveDiv,
    primitiveMod, primitiveEq, primitiveNeq, primitiveLt, primitiveGt,
    primitiveLte, primitiveGte, primitiveDisp, primitiveAnd, primitiveOr,
    primitiveXor, primitiveDup, primitiveDrop)

type ReturnStack = [Int]
type MainStack = [Int]
type Heap = [Int]
type Operands = [Char]
type ProgramCounter = Int

execLoop :: ProgramCounter -> Operands ->
    MainStack -> ReturnStack -> Heap -> Either MainStack String
execLoop pc ops ms rs hp | pc > length ops || pc < 0 =
                             Right $ "ERROR: Invalid PC = " ++ show pc ++ "."
                         | pc == length ops && null ms =
                             Right $ "OK: Empty Stack. (return stack = " ++ show rs ++ ")"
                         | pc == length ops && not (null ms) =
                             Left ms
                         | otherwise = case ord $ ops !! pc of
    0x00 -> execLoop (pc + 1) ops ms rs hp
    0x01 -> execLoop (pc + 5) ops (int : ms) rs hp
        where int = charsToInt (take 4 (drop (pc + 1) ops))
    0x02 -> execLoop newPC ops ms ((pc + 5) : rs) hp
        where newPC = charsToUInt (take 4 (drop (pc + 1) ops))
    0x04 -> case rs of
        [] -> Right "ERROR: Empty Return Stack."
        _ -> execLoop (head rs) ops ms (tail rs) hp
    0x05 -> execLoop (pc + 5 + int) ops ms rs hp
        where int = charsToInt (take 4 (drop (pc + 1) ops))
    0x06 -> case head ms of
        0 -> execLoop (pc + 5 + int) ops ms rs hp
            where int = charsToInt (take 4 (drop (pc + 1) ops))
        _ -> execLoop (pc + 5) ops ms rs hp
    0x03 -> case ord $ ops !! (pc + 1) of
        0x00 -> execLoop (pc + 2) ops (primitiveAdd ms) rs hp
        0x01 -> execLoop (pc + 2) ops (primitiveSub ms) rs hp
        0x02 -> execLoop (pc + 2) ops (primitiveMul ms) rs hp
        0x03 -> execLoop (pc + 2) ops (primitiveDiv ms) rs hp
        0x04 -> execLoop (pc + 2) ops (primitiveMod ms) rs hp
        0x05 -> execLoop (pc + 2) ops (primitiveEq ms) rs hp
        0x06 -> execLoop (pc + 2) ops (primitiveNeq ms) rs hp
        0x07 -> execLoop (pc + 2) ops (primitiveLt ms) rs hp
        0x08 -> execLoop (pc + 2) ops (primitiveGt ms) rs hp
        0x09 -> execLoop (pc + 2) ops (primitiveLte ms) rs hp
        0x0a -> execLoop (pc + 2) ops (primitiveGte ms) rs hp
        0x0b -> execLoop (pc + 2) ops (primitiveDisp ms) rs hp
        0x0c -> execLoop (pc + 2) ops (primitiveAnd ms) rs hp
        0x0d -> execLoop (pc + 2) ops (primitiveOr ms) rs hp
        0x0e -> execLoop (pc + 2) ops (primitiveXor ms) rs hp
        _ -> Right "ERROR: Invalid primitive instruction."
    0x07 -> case ms of
        [] -> Right "ERROR: Empty Stack."
        _ -> execLoop (pc + 5) ops (drop 1 ms) rs (setAt int (head ms) hp)
            where int = charsToUInt (take 4 (drop (pc + 1) ops))
    0x08 -> execLoop (pc + 5) ops ((hp !! int) : ms) rs hp
        where int = charsToUInt (take 4 (drop (pc + 1) ops))
    0x09 -> execLoop (pc + 1) ops (drop 1 ms) rs hp
    0x0a -> execLoop (pc + 1) ops (head ms : ms) rs hp
    _ -> Right "ERROR: Invalid instruction."
