{-
-- EPITECH PROJECT, 2023
-- AST.hs
-- File description:
-- Abstract Syntax Tree
-}

module Compiler where

import Parser (LuaExpr( LuaNumber, LuaArithOp, LuaVarAssign, LuaVariable, LuaReturn))
import Data.Map as Map ( Map, (!), member, insert, empty, keys, filter)

import Data.Char (ord, chr)
import Data.Bits (xor, shiftR, (.&.), complement)
import Utils (uintToChars)
import GHC.IO.Handle (hSetBinaryMode, hPutStr, hFlush, hClose)
import System.IO (openFile, IOMode(WriteMode))

type Memory = Map String Int
type Increment = Int
type Address = Int
type Bytecode = [Int]

checksum :: String -> Int
checksum = sum . map ord

luaCompile :: LuaExpr -> Bytecode
luaCompile (LuaVariable var) =
    0x08 : uintToChars (checksum var `mod` 4096)
luaCompile (LuaNumber n) = 0x01 : uintToChars n
luaCompile (LuaArithOp op a b) =
    luaCompile a ++ luaCompile b ++ [0x03, primitiveOpCode op]
luaCompile (LuaVarAssign var val) =
    luaCompile val ++ (0x07 : uintToChars (checksum var `mod` 4096))
luaCompile (LuaReturn expr) =
    luaCompile expr ++ [0x04]

primitiveOpCode :: String -> Int
primitiveOpCode "+" = 0x00  -- ADD
primitiveOpCode "-" = 0x01  -- SUB
primitiveOpCode "*" = 0x02  -- MUL
primitiveOpCode "/" = 0x03  -- DIV
primitiveOpCode "%" = 0x04  -- MOD

-- Égalité
primitiveOpCode "==" = 0x05  -- EQ
primitiveOpCode "~=" = 0x06  -- NEQ
primitiveOpCode "<" = 0x07  -- LT
primitiveOpCode ">" = 0x08  -- GT
primitiveOpCode "<=" = 0x09  -- LTE
primitiveOpCode ">=" = 0x0A  -- GTE

-- Opérations binaires
primitiveOpCode "&" = 0x0C  -- AND
primitiveOpCode "|" = 0x0D  -- OR
primitiveOpCode "~" = 0x0E  -- XOR
primitiveOpCode ">>" = 0x0F  -- RSHIFT
primitiveOpCode "<<" = 0x10  -- LSHIFT
-- TODO: Ajoutez ici d'autres cas si nécessaire
