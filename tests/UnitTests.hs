{-
-- EPITECH PROJECT, 2023
-- UnitTest.hs
-- File description:
-- Unit test
-}

module UnitTests where

import Test.Hspec

import Parser
import AST
import VM
import Compiler
import Primitives
import Utils
import VarAddress

import ParserTests
import ASTTests
import VMTests
import CompilerTests
import PrimitivesTests
import UtilsTests
import VarAddressTests

import qualified Data.Map as Map

-------------------------
-- Test pour Parser.hs --
-------------------------

main :: IO ()
main = hspec $ do

    -- ASTTests.hs
        -- sexprToAST
    test_sexpr_to_ast_with_int
    test_sexpr_to_ast_with_sym
    test_sexpr_to_ast_with_empty_list
    test_sexpr_to_ast_with_define
    test_sexpr_to_ast_with_supported_function
        -- evalAST
    test_eval_ast_with_call_and_supported_function
    test_eval_ast_with_call_and_multiple_supported_functions
    test_eval_ast_with_astsym
    test_eval_ast_with_call_and_empty_args
    test_eval_ast_with_call_and_unsupported_nested_function
    test_eval_simple_arithmetic
    test_eval_nested_arithmetic
    test_eval_division_non_zero
    test_eval_empty_SList
    test_eval_AstSym_expression
    test_eval_division_zero
        -- sumArgs
    test_sum_args_with_empty_list
    test_sum_args_with_single_arg
    test_sum_args_with_multiple_args
    test_sum_args_with_nested_calls
    test_sum_args_with_mixed_args
        -- subArgs
    test_sub_args_with_empty_list
    test_sub_args_with_single_arg
    test_sub_args_with_multiple_args
    test_sub_args_with_nested_calls
    test_sub_args_with_mixed_args
        -- divArgs
    test_div_args_with_empty_list
    test_div_args_with_single_arg
    test_div_args_with_multiple_args
    test_div_args_with_nested_calls
    test_div_args_with_mixed_args
        -- productArgs
    test_product_args_with_empty_list
    test_product_args_with_single_arg
    test_product_args_with_multiple_args
    test_product_args_with_nested_calls
    test_product_args_with_mixed_args
        -- evalInt
    test_eval_int_with_astint
    test_eval_int_with_astbool
    test_eval_int_with_astsym
    test_eval_int_with_nested_calls
    test_eval_int_with_mixed_args

    test_tableauAssociatif
    test_isString
    -- test_doDefine


    -- CompilerTests.sh
    test_checksum
    -- test_luaCompile
    test_primitiveOpCode



    -- ParserTests.hs
    test_parseChar
    test_parseDigits
    test_parseSpaces
    test_parseLuaNumber
    test_parseLuaArithOp
    test_parseLuaVarAssign
    test_parseLuaVariable
    test_parseLuaExpr

    -- test_parse_single_character
    -- test_parse_two_characters
    -- test_parse_list_of_integers
    -- test_parse_list_of_symbols
    -- test_parse_empty_input
    -- test_parse_character_not_in_input
    -- test_parse_string_with_no_digits
    -- test_parse_list_with_open_parenthesis
    -- test_parse_string_with_single_minus
    -- test_parse_nested_list
    -- test_parse_negative_integer
    -- test_parse_mixed_integers
    -- test_parse_mixed_integers_and_symbols
    -- test_parse_single_plus
    -- test_parse_single_slash

    -- VMTests.hs
    -- test_exec_loop_valid_input
    -- test_exec_loop_empty_stack
    -- test_exec_loop_non_empty_stack
    -- test_exec_loop_negative_pc
    -- test_exec_loop_pc_out_of_bounds
    -- test_multiple_instructions
        -------------------------

    -- PrimitivesTests
    test_primitive_add
    test_primitive_sub
    test_primitive_mul
    test_primitive_div
    test_primitive_mod
    test_primitive_Eq
    test_primitive_Neq
    test_primitive_Lt
    test_primitive_Gt
    test_primitive_Lte
    test_primitive_Gte
    test_primitive_Disp

    -- UtilsTests
    test_setAt
    test_charsToInt
    test_charsToUInt
    test_uintToChars

    -- VarAddressTests.hs
    -- test_transformFile
    test_transformLine
    test_updateMemory
    test_updateVariable

    -- VMTests.hs
    test_execLoop
