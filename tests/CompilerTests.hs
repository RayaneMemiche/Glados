{-
-- EPITECH PROJECT, 2023
-- CompilerTests.hs
-- File description:
-- CompilerTests
-}

module CompilerTests where

import Test.Hspec
import Compiler

import Data.Char (ord)
import Control.Exception (evaluate)

test_checksum :: Spec
test_checksum = do
  describe "checksum" $ do
    it "calculates the checksum of an empty string as 0" $ do
      checksum "" `shouldBe` 0

    it "calculates the checksum of a string" $ do
      checksum "abc" `shouldBe` (ord 'a' + ord 'b' + ord 'c')


-- test_luaCompile :: Spec
-- test_luaCompile = do
--   describe "luaCompile" $ do
--     it "compiles a LuaVariable" $ do
--       luaCompile (LuaVariable "var") `shouldBe` [0x08, ...]  -- Replace ... with expected values

--     it "compiles a LuaNumber" $ do
--       luaCompile (LuaNumber 5) `shouldBe` [0x01, ...]


test_primitiveOpCode :: Spec
test_primitiveOpCode = do
  describe "primitiveOpCode" $ do
    it "returns the opcode for addition" $ do
      primitiveOpCode "+" `shouldBe` 0x00

    it "returns the opcode for subtraction" $ do
      primitiveOpCode "-" `shouldBe` 0x01

    it "handles unknown operations" $ do
      evaluate (primitiveOpCode "unknown") `shouldThrow` anyException  -- Adjust based on how your function handles unknown ops
