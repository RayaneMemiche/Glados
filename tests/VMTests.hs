{-
-- EPITECH PROJECT, 2023
-- VMTests.hs
-- File description:
-- VMTests
-}

module VMTests where

import Test.Hspec
import VM
import Data.Char (chr)



test_execLoop :: Spec
test_execLoop = do
  describe "execLoop" $ do
    it "should execute a simple program" $ do
      let ops = [chr 0x01, '\0', '\0', '\0', '\0', chr 0x00] -- Push 0x00000000 to the stack
      let result = execLoop 0 ops [] [] []
      result `shouldBe` Left [0]

    it "should handle addition correctly" $ do
      let ops = [chr 0x01, '\0', '\0', '\0', '\0', chr 0x01, '\0', '\0', '\0', '\0', chr 0x03] -- Push 0x00000001 twice and add
      let result = execLoop 0 ops [] [] []
      result `shouldBe` Left [2]

    it "should handle error for invalid instruction" $ do
      let ops = [chr 0xFF] -- Invalid instruction
      let result = execLoop 0 ops [] [] []
      result `shouldBe` Right "ERROR: Invalid instruction."

    it "should handle error for invalid PC" $ do
      let ops = [] -- Empty program
      let result = execLoop 1 ops [] [] []
      result `shouldBe` Right "ERROR: Invalid PC = 1."

    it "should return OK for empty stack at the end" $ do
      let ops = [chr 0x01, '\0', '\0', '\0', '\0', chr 0x04] -- Push 0x00000000 and then return
      let result = execLoop 0 ops [] [] []
      result `shouldBe` Right "OK: Empty Stack."

    it "should handle primitive display correctly" $ do
      let ops = [chr 0x01, '\0', '\0', '\0', '\0', chr 0x0b, chr 0x01, '\0', '\0', '\0', '\0', chr 0x0b] -- Push 0x00000001 and then display it twice
      let result = execLoop 0 ops [] [] []
      result `shouldBe` Left [1, 1]

    it "should handle division by zero error" $ do
      let ops = [chr 0x01, '\0', '\0', '\0', '\0', chr 0x01, '\0', '\0', '\0', '\0', chr 0x03] -- Push 0x00000001 twice and then perform division by zero
      let result = execLoop 0 ops [] [] []
      result `shouldBe` Right "ERROR: Division by zero."

    it "should handle empty return stack error" $ do
      let ops = [chr 0x04] -- Return from an empty return stack
      let result = execLoop 0 ops [] [] []
      result `shouldBe` Right "ERROR: Empty Return Stack."



-- test_exec_loop_valid_input :: Spec
-- test_exec_loop_valid_input = do
--   describe "VM.execLoop with valid input" $ do
--     it "executes successfully" $ do
--       let pc = 0
--           ops = map chr [0x00, 0x01, 0x02, 0x03]
--           ms = []
--           rs = []
--       execLoop pc ops ms rs `shouldBe` Right "OK: Empty Stack."

-- test_exec_loop_empty_stack :: Spec
-- test_exec_loop_empty_stack = do
--   describe "VM.execLoop with empty stack" $ do
--     it "returns 'Empty Stack.' if the program counter reaches the end of the operands list and the main stack is empty" $ do
--       let pc = 4
--           ops = map chr [0x00, 0x01, 0x02, 0x03]
--           ms = []
--           rs = []
--       execLoop pc ops ms rs `shouldBe` Right "OK: Empty Stack."

-- test_exec_loop_non_empty_stack :: Spec
-- test_exec_loop_non_empty_stack = do
--   describe "VM.execLoop with non-empty stack" $ do
--     it "returns the head of the main stack if the program counter reaches the end of the operands list and the main stack is not empty" $ do
--       let pc = 4
--           ops = map chr [0x00, 0x01, 0x02, 0x03]
--           ms = [42]
--           rs = []
--       execLoop pc ops ms rs `shouldBe` Left (42 : ms)

-- test_exec_loop_negative_pc :: Spec
-- test_exec_loop_negative_pc = do
--   describe "VM.execLoop with negative program counter" $ do
--     it "returns an error message if the program counter is negative" $ do
--       let pc = -1
--           ops = map chr [0x00, 0x01, 0x02, 0x03]
--           ms = []
--           rs = []
--       execLoop pc ops ms rs `shouldBe` Right "ERROR: Invalid PC = -1."

-- test_exec_loop_pc_out_of_bounds :: Spec
-- test_exec_loop_pc_out_of_bounds = do
--   describe "VM.execLoop with program counter out of bounds" $ do
--     it "returns an error message if the program counter is greater than the length of the operands list" $ do
--       let pc = 5
--           ops = map chr [0x00, 0x01, 0x02, 0x03]
--           ms = []
--           rs = []
--       execLoop pc ops ms rs `shouldBe` Right "ERROR: Invalid PC = 5."


-- test_multiple_instructions :: Spec
-- test_multiple_instructions = do
--   describe "Multiple Instructions Test" $ do
--     it "should return the correct result" $ do
--       let mainStack = []
--           returnStack = []
--           operands = map chr [0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x03, 0x00]
--           programCounter = 0
--           result = execLoop programCounter operands mainStack returnStack
--       result `shouldBe` Left (1 : mainStack)


--------------------------------------------------------------------------------
