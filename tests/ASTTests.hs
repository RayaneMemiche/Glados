{-
-- EPITECH PROJECT, 2023
-- ASTTtests.hs
-- File description:
-- ASTTtests
-}

module ASTTests where

import Test.Hspec
import AST
import qualified Data.Map as Map

code_under_test :: Ast -> Maybe Ast
code_under_test = evalAST


-- -------------------------
-- sexprToAST
-- -------------------------


test_sexpr_to_ast_with_int :: Spec
test_sexpr_to_ast_with_int = do
  describe "sexprToAST with SInt" $ do
    it "returns Just AstInt" $ do
      let result = sexprToAST (SInt 42)
      result `shouldBe` Just (AstInt 42)

test_sexpr_to_ast_with_sym :: Spec
test_sexpr_to_ast_with_sym = do
  describe "sexprToAST with SSym" $ do
    it "returns Just AstSym" $ do
      let result = sexprToAST (SSym "variable")
      result `shouldBe` Just (AstSym "variable")

test_sexpr_to_ast_with_empty_list :: Spec
test_sexpr_to_ast_with_empty_list = do
  describe "sexprToAST with SList []" $ do
    it "returns Nothing" $ do
      let result = sexprToAST (SList [])
      result `shouldBe` Nothing

test_sexpr_to_ast_with_define :: Spec
test_sexpr_to_ast_with_define = do
  describe "sexprToAST with SList [SSym \"define\", SSym \"variable\", SInt 42]" $ do
    it "returns Just Define" $ do
      let result = sexprToAST (SList [SSym "define", SSym "variable", SInt 42])
      result `shouldBe` Just (Define "variable" (AstInt 42))

test_sexpr_to_ast_with_supported_function :: Spec
test_sexpr_to_ast_with_supported_function = do
  describe "sexprToAST with SList [SSym \"+\", SInt 2, SInt 3]" $ do
    it "returns Just Call" $ do
      let result = sexprToAST (SList [SSym "+", SInt 2, SInt 3])
      result `shouldBe` Just (Call (AstSym "+") [AstInt 2, AstInt 3])


-- -------------------------
-- evalAST
-- -------------------------

test_eval_ast_with_call_and_supported_function :: Spec
test_eval_ast_with_call_and_supported_function = do
  describe "evalAST with Call and supported function" $ do
    it "returns Just AstInt" $ do
      code_under_test (Call (AstSym "+") [AstInt 2, AstInt 3]) `shouldBe` Just (AstInt 5)

test_eval_ast_with_call_and_multiple_supported_functions :: Spec
test_eval_ast_with_call_and_multiple_supported_functions = do
  describe "evalAST with Call and multiple supported functions" $ do
    it "returns Just AstInt" $ do
      code_under_test (Call (AstSym "*") [AstInt 4, AstInt 5]) `shouldBe` Just (AstInt 20)

test_eval_ast_with_astsym :: Spec
test_eval_ast_with_astsym = do
  describe "evalAST with AstSym" $ do
    it "returns Nothing" $ do
      code_under_test (AstSym "non_supported") `shouldBe` Nothing

test_eval_ast_with_call_and_empty_args :: Spec
test_eval_ast_with_call_and_empty_args = do
  describe "evalAST with Call and empty args" $ do
    it "returns Nothing" $ do
      code_under_test (Call (AstSym "+") []) `shouldBe` Nothing

test_eval_ast_with_call_and_unsupported_nested_function :: Spec
test_eval_ast_with_call_and_unsupported_nested_function = do
  describe "evalAST with Call and unsupported nested function" $ do
    it "returns Nothing" $ do
      code_under_test (Call (AstSym "-") [AstInt 6, Call (AstSym "*") [AstInt 4, AstInt 3]]) `shouldBe` Nothing

test_eval_simple_arithmetic :: Spec
test_eval_simple_arithmetic = do
  describe "evalAST simple arithmetic" $ do
    it "evaluates simple addition" $ do
      let expr = Call (AstSym "+") [AstInt 2, AstInt 3]
      let result = evalAST expr
      result `shouldBe` Just (AstInt 5)

test_eval_nested_arithmetic :: Spec
test_eval_nested_arithmetic = do
  describe "evalAST nested arithmetic" $ do
    it "evaluates nested arithmetic expression" $ do
      let expr = Call (AstSym "*") [Call (AstSym "+") [AstInt 4, AstInt 3], AstInt 6]
      let result = evalAST expr
      result `shouldBe` Just (AstInt 42)

test_eval_division_non_zero :: Spec
test_eval_division_non_zero = do
  describe "evalAST division by non-zero" $ do
    it "evaluates division by a non-zero number" $ do
      let expr = Call (AstSym "/") [AstInt 10, AstInt 2]
      let result = evalAST expr
      result `shouldBe` Just (AstInt 5)

test_eval_empty_SList :: Spec
test_eval_empty_SList = do
  describe "sexprToAST with empty SList" $ do
    it "returns Nothing for empty SList" $ do
      let expr = SList []
      let result = sexprToAST expr
      result `shouldBe` Nothing

test_eval_AstSym_expression :: Spec
test_eval_AstSym_expression = do
  describe "evalAST with AstSym" $ do
    it "returns Nothing for non-supported AstSym" $ do
      let expr = AstSym "non_supported"
      let result = evalAST expr
      result `shouldBe` Nothing

test_eval_division_zero :: Spec
test_eval_division_zero = do
  describe "evalAST division by zero" $ do
    it "returns Nothing for division by zero" $ do
      let expr = Call (AstSym "/") [AstInt 10, AstInt 0]
      let result = evalAST expr
      result `shouldBe` Nothing



-- -------------------------
-- sumArgs
-- -------------------------


test_sum_args_with_empty_list :: Spec
test_sum_args_with_empty_list = do
  describe "sumArgs with empty list" $ do
    it "returns Nothing" $ do
      let result = sumArgs []
      result `shouldBe` Nothing

test_sum_args_with_single_arg :: Spec
test_sum_args_with_single_arg = do
  describe "sumArgs with a single argument" $ do
    it "returns the same argument" $ do
      let result = sumArgs [AstInt 42]
      result `shouldBe` Just (AstInt 42)

test_sum_args_with_multiple_args :: Spec
test_sum_args_with_multiple_args = do
  describe "sumArgs with multiple arguments" $ do
    it "returns the sum of arguments" $ do
      let result = sumArgs [AstInt 2, AstInt 3, AstInt 5]
      result `shouldBe` Just (AstInt 10)

test_sum_args_with_nested_calls :: Spec
test_sum_args_with_nested_calls = do
  describe "sumArgs with nested calls" $ do
    it "returns the sum of evaluated calls" $ do
      let result = sumArgs [Call (AstSym "+") [AstInt 2, AstInt 3], Call (AstSym "+") [AstInt 4, AstInt 6]]
      result `shouldBe` Just (AstInt 15)

test_sum_args_with_mixed_args :: Spec
test_sum_args_with_mixed_args = do
  describe "sumArgs with mixed arguments" $ do
    it "returns the sum of evaluated arguments" $ do
      let result = sumArgs [AstInt 2, Call (AstSym "+") [AstInt 3, AstInt 5], AstInt 4]
      result `shouldBe` Just (AstInt 14)



-- -------------------------
-- subArgs
-- -------------------------


test_sub_args_with_empty_list :: Spec
test_sub_args_with_empty_list = do
  describe "subArgs with empty list" $ do
    it "returns Nothing" $ do
      let result = subArgs []
      result `shouldBe` Nothing

test_sub_args_with_single_arg :: Spec
test_sub_args_with_single_arg = do
  describe "subArgs with a single argument" $ do
    it "returns the same argument" $ do
      let result = subArgs [AstInt 42]
      result `shouldBe` Just (AstInt 42)

test_sub_args_with_multiple_args :: Spec
test_sub_args_with_multiple_args = do
  describe "subArgs with multiple arguments" $ do
    it "returns the subtraction of arguments" $ do
      let result = subArgs [AstInt 10, AstInt 3, AstInt 2]
      result `shouldBe` Just (AstInt 5)

test_sub_args_with_nested_calls :: Spec
test_sub_args_with_nested_calls = do
  describe "subArgs with nested calls" $ do
    it "returns the subtraction of evaluated calls" $ do
      let result = subArgs [Call (AstSym "-") [AstInt 10, AstInt 3], Call (AstSym "-") [AstInt 6, AstInt 4]]
      result `shouldBe` Just (AstInt 7)

test_sub_args_with_mixed_args :: Spec
test_sub_args_with_mixed_args = do
  describe "subArgs with mixed arguments" $ do
    it "returns the subtraction of evaluated arguments" $ do
      let result = subArgs [AstInt 20, Call (AstSym "-") [AstInt 10, AstInt 3], AstInt 5]
      result `shouldBe` Just (AstInt 12)



-- -------------------------
-- divArgs
-- -------------------------


test_div_args_with_empty_list :: Spec
test_div_args_with_empty_list = do
  describe "divArgs with empty list" $ do
    it "returns Nothing" $ do
      let result = divArgs []
      result `shouldBe` Nothing

test_div_args_with_single_arg :: Spec
test_div_args_with_single_arg = do
  describe "divArgs with a single argument" $ do
    it "returns the same argument" $ do
      let result = divArgs [AstInt 42]
      result `shouldBe` Just (AstInt 42)

test_div_args_with_multiple_args :: Spec
test_div_args_with_multiple_args = do
  describe "divArgs with multiple arguments" $ do
    it "returns the division of arguments" $ do
      let result = divArgs [AstInt 10, AstInt 2, AstInt 5]
      result `shouldBe` Just (AstInt 1)

test_div_args_with_nested_calls :: Spec
test_div_args_with_nested_calls = do
  describe "divArgs with nested calls" $ do
    it "returns the division of evaluated calls" $ do
      let result = divArgs [Call (AstSym "/") [AstInt 10, AstInt 2], Call (AstSym "/") [AstInt 6, AstInt 3]]
      result `shouldBe` Just (AstInt 5)

test_div_args_with_mixed_args :: Spec
test_div_args_with_mixed_args = do
  describe "divArgs with mixed arguments" $ do
    it "returns the division of evaluated arguments" $ do
      let result = divArgs [AstInt 20, Call (AstSym "/") [AstInt 10, AstInt 2], AstInt 5]
      result `shouldBe` Just (AstInt 2)


-- -------------------------
-- productArgs
-- -------------------------


test_product_args_with_empty_list :: Spec
test_product_args_with_empty_list = do
  describe "productArgs with empty list" $ do
    it "returns Nothing" $ do
      let result = productArgs []
      result `shouldBe` Nothing

test_product_args_with_single_arg :: Spec
test_product_args_with_single_arg = do
  describe "productArgs with a single argument" $ do
    it "returns the same argument" $ do
      let result = productArgs [AstInt 42]
      result `shouldBe` Just (AstInt 42)

test_product_args_with_multiple_args :: Spec
test_product_args_with_multiple_args = do
  describe "productArgs with multiple arguments" $ do
    it "returns the product of arguments" $ do
      let result = productArgs [AstInt 4, AstInt 5]
      result `shouldBe` Just (AstInt 20)

test_product_args_with_nested_calls :: Spec
test_product_args_with_nested_calls = do
  describe "productArgs with nested calls" $ do
    it "returns the product of evaluated calls" $ do
      let result = productArgs [Call (AstSym "*") [AstInt 4, AstInt 3], Call (AstSym "*") [AstInt 2, AstInt 5]]
      result `shouldBe` Just (AstInt 120)

test_product_args_with_mixed_args :: Spec
test_product_args_with_mixed_args = do
  describe "productArgs with mixed arguments" $ do
    it "returns the product of evaluated arguments" $ do
      let result = productArgs [AstInt 20, Call (AstSym "*") [AstInt 4, AstInt 3], AstInt 5]
      result `shouldBe` Just (AstInt 120)


-- -------------------------
-- evalInt
-- -------------------------


test_eval_int_with_astint :: Spec
test_eval_int_with_astint = do
  describe "evalInt with AstInt" $ do
    it "returns the integer value" $ do
      let result = evalInt (AstInt 42)
      result `shouldBe` Just 42

test_eval_int_with_astbool :: Spec
test_eval_int_with_astbool = do
  describe "evalInt with AstBool" $ do
    it "returns Nothing" $ do
      let result = evalInt (AstBool True)
      result `shouldBe` Nothing

test_eval_int_with_astsym :: Spec
test_eval_int_with_astsym = do
  describe "evalInt with AstSym" $ do
    it "returns Nothing" $ do
      let result = evalInt (AstSym "variable")
      result `shouldBe` Nothing

test_eval_int_with_nested_calls :: Spec
test_eval_int_with_nested_calls = do
  describe "evalInt with nested calls" $ do
    it "returns the integer value of the inner call result" $ do
      let result = evalInt (Call (AstSym "+") [AstInt 2, AstInt 3])
      result `shouldBe` Just 5

test_eval_int_with_mixed_args :: Spec
test_eval_int_with_mixed_args = do
  describe "evalInt with mixed arguments" $ do
    it "returns the integer value of the evaluated argument" $ do
      let result = evalInt (Call (AstSym "+") [AstInt 4, Call (AstSym "*") [AstInt 3, AstInt 2]])
      result `shouldBe` Just 10


-- -------------------------
-- test_tableauAssociatif
-- -------------------------

test_tableauAssociatif :: Spec
test_tableauAssociatif = do
  describe "tableauAssociatif" $ do
    it "contains specific key-value pairs" $ do
      let expectedMap = Map.fromList [(SSym "a", SSym "1"), (SSym "b", SInt 1), (SSym "a", SSym "toto")]
      tableauAssociatif `shouldBe` expectedMap

-- -------------------------
-- isString
-- -------------------------

test_isString :: Spec
test_isString = do
  describe "isString" $ do
    it "returns True for SSym" $ do
      isString (SSym "test") `shouldBe` True

    it "returns False for non-SSym values" $ do
      isString (SInt 5) `shouldBe` False
      isString (SList [SSym "a", SInt 1]) `shouldBe` False


-- -------------------------
-- doDefine
-- -------------------------

-- test_doDefine :: Spec
-- test_doDefine = do
--   describe "doDefine" $ do
--     it "adds new key-value pair to the map" $ do
--       let initialMap = Map.fromList [(SSym "x", SInt 1)]
--       let result = doDefine initialMap (SSym "y") (SInt 2)
--       result `shouldBe` Just (Map.fromList [(SSym "x", SInt 1), (SSym "y", SInt 2)])

--     it "returns Nothing if key already exists" $ do
--       let initialMap = Map.fromList [(SSym "x", SInt 1)]
--       let result = doDefine initialMap (SSym "x") (SInt 3)
--       result `shouldBe` Nothing  -- Depending on how you handle errors, this could also be an error case

--     it "returns an error if the first SExpr is not SSym" $ do
--       let initialMap = Map.fromList [(SSym "x", SInt 1)]
--       evaluate (doDefine initialMap (SInt 5) (SInt 3)) `shouldThrow` anyErrorCall
