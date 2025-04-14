{-
-- EPITECH PROJECT, 2023
-- VarAddressTests.hs
-- File description:
-- VarAddressTests
-}

module VarAddressTests where

import Test.Hspec

import VarAddress
import qualified Data.Map as Map
import Control.Monad.State

-- test_transformFile :: Spec
-- test_transformFile = hspec $ do
--     describe "transformFile" $ do
--         it "should transform a file and write the transformed content to output.txt" $ do
--             writeFile "test_input.txt" "CALL my_var\nLOAD my_var\n"
--             transformFile "test_input.txt"
--             transformedContent <- readFile "output.txt"
--             transformedContent `shouldBe` "CALL 0 my_var\nLOAD 1 my_var\n"
--             removeFile "test_input.txt"
--             removeFile "output.txt"


test_transformLine :: Spec
test_transformLine = do
    describe "transformLine" $ do
        it "should transform a line with an instruction and variable" $ do
            let line = "CALL my_var"
            let transformedLine = evalState (transformLine line) 0
            transformedLine `shouldBe` "CALL 0 my_var"


test_updateMemory :: Spec
test_updateMemory = do
    describe "updateMemory" $ do
        it "should update memory for CALL instruction" $ do
            let instruction = "CALL"
            let variable = "my_var"
            let currentAddress = 0
            let newAddress = evalState (updateMemory instruction variable currentAddress) 0
            newAddress `shouldBe` 0

        it "should update memory for LOAD instruction" $ do
            let instruction = "LOAD"
            let variable = "my_var"
            let currentAddress = 0
            let newAddress = evalState (updateMemory instruction variable currentAddress) 0
            newAddress `shouldBe` 0

test_updateVariable :: Spec
test_updateVariable = do
    describe "updateVariable" $ do
        it "should update variable address" $ do
            let variable = "my_var"
            let currentAddress = 0
            let newAddress = evalState (updateVariable variable currentAddress) 0
            newAddress `shouldBe` 0
