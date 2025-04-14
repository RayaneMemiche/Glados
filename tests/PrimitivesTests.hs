{-
-- EPITECH PROJECT, 2023
-- PrimitivesTests.hs
-- File description:
-- PrimitivesTests
-}

module PrimitivesTests where

import Test.Hspec
import Primitives


-- Test cases for primitiveAdd
test_primitive_add :: Spec
test_primitive_add = do
  describe "VM.primitiveAdd" $ do
    it "should add two values on the stack" $ do
      let ms = [2, 3, 4]
      primitiveAdd ms `shouldBe` [5, 4]

    it "should handle a single value on the stack" $ do
      let ms = [42]
      primitiveAdd ms `shouldBe` [42]

    it "should handle an empty stack" $ do
      let ms = []
      primitiveAdd ms `shouldBe` []

    it "should handle negative numbers" $ do
      let ms = [-2, 3, 4]
      primitiveAdd ms `shouldBe` [1, 4]


-- Test cases for primitiveSub
test_primitive_sub :: Spec
test_primitive_sub = do
  describe "VM.primitiveSub" $ do
    it "should subtract two values on the stack" $ do
      let ms = [5, 3, 4]
      primitiveSub ms `shouldBe` [2, 4]

    it "should handle a single value on the stack" $ do
      let ms = [42]
      primitiveSub ms `shouldBe` [42]

    it "should handle an empty stack" $ do
      let ms = []
      primitiveSub ms `shouldBe` []

    it "should handle negative results" $ do
      let ms = [2, 3, 4]
      primitiveSub ms `shouldBe` [1, 4]

-- Test cases for primitiveMul
test_primitive_mul :: Spec
test_primitive_mul = do
  describe "VM.primitiveMul" $ do
    it "should multiply two values on the stack" $ do
      let ms = [2, 3, 4]
      primitiveMul ms `shouldBe` [6, 4]

    it "should handle a single value on the stack" $ do
      let ms = [42]
      primitiveMul ms `shouldBe` [42]

    it "should handle an empty stack" $ do
      let ms = []
      primitiveMul ms `shouldBe` []

    it "should handle multiplication by zero" $ do
      let ms = [2, 0, 4]
      primitiveMul ms `shouldBe` [0, 4]

-- Test cases for primitiveDiv
test_primitive_div :: Spec
test_primitive_div = do
  describe "VM.primitiveDiv" $ do
    it "should divide two values on the stack" $ do
      let ms = [6, 3, 4]
      primitiveDiv ms `shouldBe` [2, 4]

    it "should handle a single value on the stack" $ do
      let ms = [42]
      primitiveDiv ms `shouldBe` [42]

    it "should handle an empty stack" $ do
      let ms = []
      primitiveDiv ms `shouldBe` []

    it "should handle division by zero" $ do
      let ms = [2, 0, 4]
      primitiveDiv ms `shouldBe` [0, 4]

-- Test cases for primitiveMod
test_primitive_mod :: Spec
test_primitive_mod = do
  describe "VM.primitiveMod" $ do
    it "should calculate the modulo of two values on the stack" $ do
      let ms = [7, 3, 4]
      primitiveMod ms `shouldBe` [1, 4]

    it "should handle a single value on the stack" $ do
      let ms = [42]
      primitiveMod ms `shouldBe` [42]

    it "should handle an empty stack" $ do
      let ms = []
      primitiveMod ms `shouldBe` []

    it "should handle modulo by zero" $ do
      let ms = [2, 0, 4]
      primitiveMod ms `shouldBe` [0, 4]


test_primitive_Eq :: Spec
test_primitive_Eq = do
  describe "primitiveEq" $ do
    it "should return [1] when top two stack elements are equal" $ do
      let stack = [5, 5, 10]
      let result = primitiveEq stack
      result `shouldBe` [1, 10]

    it "should return [0] when top two stack elements are not equal" $ do
      let stack = [5, 7, 10]
      let result = primitiveEq stack
      result `shouldBe` [0, 10]

    it "should return [0] when the stack is empty" $ do
      let stack = []
      let result = primitiveEq stack
      result `shouldBe` []

    it "should return [1] when there's only one element in the stack" $ do
      let stack = [42]
      let result = primitiveEq stack
      result `shouldBe` [1]

    it "should handle negative numbers correctly" $ do
      let stack = [-5, -5, 10]
      let result = primitiveEq stack
      result `shouldBe` [1, 10]

    it "should handle mixed positive and negative numbers" $ do
      let stack = [-5, 5, 10]
      let result = primitiveEq stack
      result `shouldBe` [0, 10]


test_primitive_Neq :: Spec
test_primitive_Neq = do
  describe "primitiveNeq" $ do
    it "should return [0] when top two stack elements are equal" $ do
      let stack = [5, 5, 10]
      let result = primitiveNeq stack
      result `shouldBe` [0, 10]

    it "should return [1] when top two stack elements are not equal" $ do
      let stack = [5, 7, 10]
      let result = primitiveNeq stack
      result `shouldBe` [1, 10]

    it "should return [0] when the stack is empty" $ do
      let stack = []
      let result = primitiveNeq stack
      result `shouldBe` []

    it "should return [0] when there's only one element in the stack" $ do
      let stack = [42]
      let result = primitiveNeq stack
      result `shouldBe` [0]

    it "should handle negative numbers correctly" $ do
      let stack = [-5, -5, 10]
      let result = primitiveNeq stack
      result `shouldBe` [0, 10]

    it "should handle mixed positive and negative numbers" $ do
      let stack = [-5, 5, 10]
      let result = primitiveNeq stack
      result `shouldBe` [1, 10]


test_primitive_Lt :: Spec
test_primitive_Lt = do
  describe "primitiveLt" $ do
    it "should return [0] when top two stack elements are equal" $ do
      let stack = [5, 5, 10]
      let result = primitiveLt stack
      result `shouldBe` [0, 10]

    it "should return [1] when the first element is less than the second" $ do
      let stack = [5, 7, 10]
      let result = primitiveLt stack
      result `shouldBe` [1, 10]

    it "should return [0] when the first element is greater than the second" $ do
      let stack = [7, 5, 10]
      let result = primitiveLt stack
      result `shouldBe` [0, 10]

    it "should return [0] when the stack is empty" $ do
      let stack = []
      let result = primitiveLt stack
      result `shouldBe` []

    it "should return [0] when there's only one element in the stack" $ do
      let stack = [42]
      let result = primitiveLt stack
      result `shouldBe` [0]

    it "should handle negative numbers correctly" $ do
      let stack = [-5, -3, 10]
      let result = primitiveLt stack
      result `shouldBe` [1, 10]

    it "should handle mixed positive and negative numbers" $ do
      let stack = [-5, 5, 10]
      let result = primitiveLt stack
      result `shouldBe` [1, 10]



test_primitive_Gt :: Spec
test_primitive_Gt = do
  describe "primitiveGt" $ do
    it "should return [0] when top two stack elements are equal" $ do
      let stack = [5, 5, 10]
      let result = primitiveGt stack
      result `shouldBe` [0, 10]

    it "should return [0] when the first element is less than the second" $ do
      let stack = [5, 7, 10]
      let result = primitiveGt stack
      result `shouldBe` [0, 10]

    it "should return [1] when the first element is greater than the second" $ do
      let stack = [7, 5, 10]
      let result = primitiveGt stack
      result `shouldBe` [1, 10]

    it "should return [0] when the stack is empty" $ do
      let stack = []
      let result = primitiveGt stack
      result `shouldBe` []

    it "should return [0] when there's only one element in the stack" $ do
      let stack = [42]
      let result = primitiveGt stack
      result `shouldBe` [0]

    it "should handle negative numbers correctly" $ do
      let stack = [-5, -3, 10]
      let result = primitiveGt stack
      result `shouldBe` [0, 10]

    it "should handle mixed positive and negative numbers" $ do
      let stack = [-5, 5, 10]
      let result = primitiveGt stack
      result `shouldBe` [0, 10]


test_primitive_Lte :: Spec
test_primitive_Lte = do
  describe "primitiveLte" $ do
    it "should return [1] when top two stack elements are equal" $ do
      let stack = [5, 5, 10]
      let result = primitiveLte stack
      result `shouldBe` [1, 10]

    it "should return [1] when the first element is less than the second" $ do
      let stack = [5, 7, 10]
      let result = primitiveLte stack
      result `shouldBe` [1, 10]

    it "should return [0] when the first element is greater than the second" $ do
      let stack = [7, 5, 10]
      let result = primitiveLte stack
      result `shouldBe` [0, 10]

    it "should return [1] when the stack is empty" $ do
      let stack = []
      let result = primitiveLte stack
      result `shouldBe` []

    it "should return [1] when there's only one element in the stack" $ do
      let stack = [42]
      let result = primitiveLte stack
      result `shouldBe` [1]

    it "should handle negative numbers correctly" $ do
      let stack = [-5, -3, 10]
      let result = primitiveLte stack
      result `shouldBe` [1, 10]

    it "should handle mixed positive and negative numbers" $ do
      let stack = [-5, 5, 10]
      let result = primitiveLte stack
      result `shouldBe` [1, 10]

    it "should return [0] when top two stack elements are equal (negative)" $ do
      let stack = [-5, -5, 10]
      let result = primitiveLte stack
      result `shouldBe` [1, 10]

    it "should return [1] when the first element is less than the second (negative)" $ do
      let stack = [-7, -5, 10]
      let result = primitiveLte stack
      result `shouldBe` [1, 10]

    it "should return [0] when the first element is greater than the second (negative)" $ do
      let stack = [-5, -7, 10]
      let result = primitiveLte stack
      result `shouldBe` [1, 10]


test_primitive_Gte :: Spec
test_primitive_Gte = do
  describe "primitiveGte" $ do
    it "should return [1] when top two stack elements are equal" $ do
      let stack = [5, 5, 10]
      let result = primitiveGte stack
      result `shouldBe` [1, 10]

    it "should return [1] when the first element is greater than the second" $ do
      let stack = [7, 5, 10]
      let result = primitiveGte stack
      result `shouldBe` [1, 10]

    it "should return [0] when the first element is less than the second" $ do
      let stack = [5, 7, 10]
      let result = primitiveGte stack
      result `shouldBe` [0, 10]

    it "should return [1] when the stack is empty" $ do
      let stack = []
      let result = primitiveGte stack
      result `shouldBe` []

    it "should return [1] when there's only one element in the stack" $ do
      let stack = [42]
      let result = primitiveGte stack
      result `shouldBe` [1]

    it "should handle negative numbers correctly" $ do
      let stack = [-5, -3, 10]
      let result = primitiveGte stack
      result `shouldBe` [1, 10]

    it "should handle mixed positive and negative numbers" $ do
      let stack = [-5, 5, 10]
      let result = primitiveGte stack
      result `shouldBe` [0, 10]

    it "should return [1] when top two stack elements are equal (negative)" $ do
      let stack = [-5, -5, 10]
      let result = primitiveGte stack
      result `shouldBe` [1, 10]

    it "should return [1] when the first element is greater than the second (negative)" $ do
      let stack = [-5, -7, 10]
      let result = primitiveGte stack
      result `shouldBe` [1, 10]

    it "should return [0] when the first element is less than the second (negative)" $ do
      let stack = [-7, -5, 10]
      let result = primitiveGte stack
      result `shouldBe` [0, 10]


test_primitive_Disp :: Spec
test_primitive_Disp = do
  describe "primitiveDisp" $ do
    it "should return the same stack when not empty" $ do
      let stack = [1, 2, 3]
      let result = primitiveDisp stack
      result `shouldBe` [1, 2, 3]

    it "should return an empty stack when input is empty" $ do
      let stack = []
      let result = primitiveDisp stack
      result `shouldBe` []


test_primitive_And :: Spec
test_primitive_And = do
  describe "primitiveAnd" $ do
    it "should perform bitwise AND on the top two elements" $ do
      let stack = [1, 3, 5] -- 1 AND 3 = 1
      let result = primitiveAnd stack
      result `shouldBe` [1, 5]

    it "should return the same stack if only one element" $ do
      let stack = [2]
      let result = primitiveAnd stack
      result `shouldBe` [2]

    it "should return an empty stack when input is empty" $ do
      let stack = []
      let result = primitiveAnd stack
      result `shouldBe` []


test_primitive_Or :: Spec
test_primitive_Or = do
  describe "primitiveOr" $ do
    it "should perform bitwise OR on the top two elements" $ do
      let stack = [1, 2, 3] -- 1 OR 2 = 3
      let result = primitiveOr stack
      result `shouldBe` [3, 3]



test_primitive_Xor :: Spec
test_primitive_Xor = do
  describe "primitiveXor" $ do
    it "should perform bitwise XOR on the top two elements" $ do
      let stack = [1, 3, 5] -- 1 XOR 3 = 2
      let result = primitiveXor stack
      result `shouldBe` [2, 5]


-- test_primitive_Rshift :: Spec
-- test_primitive_Rshift = do
--   describe "primitiveRshift" $ do
--     it "should perform right bitwise shift on the top two elements" $ do
--         let stack = [2, 4, 8] -- 4 >> 2 = 1
--         let result = primitiveRshift stack
--         result shouldBe [1, 8]

--     it "should return the same stack if only one element" $ do
--         let stack = [2]
--         let result = primitiveRshift stack
--         result `shouldBe` [2]

--     it "should return an empty stack when input is empty" $ do
--         let stack = []
--         let result = primitiveRshift stack
--         result `shouldBe` []


-- test_primitive_Lshift :: Spec
-- test_primitive_Lshift = do
--   describe "primitiveLshift" $ do
--     it "should perform left bitwise shift on the top two elements" $ do
--       let stack = [1, 4, 8] -- 4 << 1 = 8
--       let result = primitiveLshift stack
--       result `shouldBe` [8, 8]

--     it "should return the same stack if only one element" $ do
--       let stack = [2]
--       let result = primitiveLshift stack
--       result `shouldBe` [2]

--     it "should return an empty stack when input is empty" $ do
--       let stack = []
--       let result = primitiveLshift stack
--       result `shouldBe` []

