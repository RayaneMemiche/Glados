{-
-- EPITECH PROJECT, 2023
-- ParserTests.hs
-- File description:
-- ParserTests
-}

module ParserTests where
import Test.Hspec
import Parser
import AST

test_parseChar :: Spec
test_parseChar = do
    describe "parseChar" $ do
        it "parses a specific character" $ do
            runParser (parseChar 'A') "ABC" `shouldBe` Just ('A', "BC")

test_parseDigits :: Spec
test_parseDigits = do
    describe "parseDigits" $ do
        it "parses a sequence of digits" $ do
            runParser parseDigits "123ABC" `shouldBe` Just ("123", "ABC")

test_parseSpaces :: Spec
test_parseSpaces = do
    describe "parseSpace" $ do
        it "parses spaces" $ do
            runParser parseSpace "  ABC" `shouldBe` Just ((), "ABC")

test_parseLuaNumber :: Spec
test_parseLuaNumber = do
    describe "parseLuaNumber" $ do
        it "parses a Lua number" $ do
            runParser parseLuaNumber "123 ABC" `shouldBe` Just (LuaNumber 123, " ABC")

test_parseLuaArithOp :: Spec
test_parseLuaArithOp = do
    describe "parseLuaArithOp" $ do
        it "parses a Lua arithmetic operation" $ do
            runParser parseLuaArithOp "1 + 2" `shouldBe` Just (LuaArithOp "+" (LuaNumber 1) (LuaNumber 2), "")

test_parseLuaVarAssign :: Spec
test_parseLuaVarAssign = do
    describe "parseLuaVarAssign" $ do
        it "parses a Lua variable assignment" $ do
            runParser parseLuaVarAssign "local x = 42" `shouldBe` Just (LuaVarAssign "x" (LuaNumber 42), "")

test_parseLuaVariable :: Spec
test_parseLuaVariable = do
    describe "parseLuaVariable" $ do
        it "parses a Lua variable" $ do
            runParser parseLuaVariable "variableName" `shouldBe` Just (LuaVariable "variableName", "")

test_parseLuaExpr :: Spec
test_parseLuaExpr = do
    describe "parseLuaExpr" $ do
        it "parses a Lua expression" $ do
            runParser parseLuaExpr "local x = 42" `shouldBe` Just (LuaVarAssign "x" (LuaNumber 42), "")
            runParser parseLuaExpr "1 + 2" `shouldBe` Just (LuaArithOp "+" (LuaNumber 1) (LuaNumber 2), "")



-- test_parse_single_character :: Spec
-- test_parse_single_character = do
--   describe "parseChar" $ do
--     it "should parse a single character" $ do
--       let result = runParser (parseChar 'a') "abc"
--       result `shouldBe` Just ('a', "bc")
--     it "should return Nothing when parsing a character not in the input" $ do
--       let result = runParser (parseChar 'b') "abc"
--       result `shouldBe` Nothing

-- test_parse_two_characters :: Spec
-- test_parse_two_characters = do
--   describe "parseAnd" $ do
--     it "should parse two characters" $ do
--       let result = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
--       result `shouldBe` Just (('a', 'b'), "cd")

-- test_parse_list_of_integers :: Spec
-- test_parse_list_of_integers = do
--   describe "parseList" $ do
--     it "should parse a list of integers" $ do
--       let result = runParser (parseList parseInt) "(1 2 3 4)"
--       result `shouldBe` Just ([1, 2, 3, 4], "")

-- test_parse_list_of_symbols :: Spec
-- test_parse_list_of_symbols = do
--   describe "parseList" $ do
--     it "should parse a list of symbols" $ do
--       let result = runParser (parseList parseSymbol) "(a b c)"
--       result `shouldBe` Just ([SSym "a", SSym "b", SSym "c"], "")

-- test_parse_empty_input :: Spec
-- test_parse_empty_input = do
--   describe "runParser" $ do
--     it "should return Nothing when parsing an empty input" $ do
--       let result = runParser (parseChar 'a') ""
--       result `shouldBe` Nothing

-- test_parse_character_not_in_input :: Spec
-- test_parse_character_not_in_input = do
--   describe "runParser" $ do
--     it "should return Nothing when parsing a character not in the input" $ do
--       let result = runParser (parseChar 'a') "bcd"
--       result `shouldBe` Nothing

-- test_parse_string_with_no_digits :: Spec
-- test_parse_string_with_no_digits = do
--   describe "runParser" $ do
--     it "should return Nothing when parsing a string with no digits" $ do
--       let result = runParser parseInt "abc"
--       result `shouldBe` Nothing

-- test_parse_list_with_open_parenthesis :: Spec
-- test_parse_list_with_open_parenthesis = do
--   describe "runParser" $ do
--     it "should return Nothing when parsing a list with an open parenthesis but no closing parenthesis" $ do
--       let result = runParser (parseList parseInt) "(1 2 3"
--       result `shouldBe` Nothing

-- test_parse_string_with_single_minus :: Spec
-- test_parse_string_with_single_minus = do
--   describe "runParser" $ do
--     it "should return Nothing when parsing a string with a single minus" $ do
--       let result = runParser parseInt "-"
--       result `shouldBe` Nothing

-- test_parse_nested_list :: Spec
-- test_parse_nested_list = do
--   describe "runParser" $ do
--     it "should return Nothing when parsing a nested list" $ do
--       let result = runParser (parseList parseInt) "(1 (2 3) 4)"
--       result `shouldBe` Nothing

-- test_parse_negative_integer :: Spec
-- test_parse_negative_integer = do
--   describe "runParser" $ do
--     it "should parse a negative integer" $ do
--       let result = runParser parseInt "-1"
--       result `shouldBe` Just (-1, "")

-- test_parse_mixed_integers :: Spec
-- test_parse_mixed_integers = do
--   describe "runParser" $ do
--     it "should parse a list of mixed integers" $ do
--       let result = runParser (parseList parseInt) "(1 -2 3 -4)"
--       result `shouldBe` Just ([1, -2, 3, -4], "")

-- test_parse_mixed_integers_and_symbols :: Spec
-- test_parse_mixed_integers_and_symbols = do
--   describe "runParser" $ do
--     it "should parse a list of mixed integers and symbols" $ do
--       let result = runParser (parseList parseSymbol) "(a 1 b -2 c 3 d -4)"
--       result `shouldBe` Just ([SSym "a", SInt 1, SSym "b", SInt (-2), SSym "c", SInt 3, SSym "d", SInt (-4)], "")

-- test_parse_single_plus :: Spec
-- test_parse_single_plus = do
--   describe "runParser" $ do
--     it "should parse a single plus" $ do
--       let result = runParser parseSymbol "+"
--       result `shouldBe` Just (SSym "+", "")

-- test_parse_single_slash :: Spec
-- test_parse_single_slash = do
--   describe "runParser" $ do
--     it "should parse a single slash" $ do
--       let result = runParser parseSymbol "/"
--       result `shouldBe` Just (SSym "/", "")