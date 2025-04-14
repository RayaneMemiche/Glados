{-
-- EPITECH PROJECT, 2023
-- Parser.hs
-- File description:
-- Parser module
-}

module Parser where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit)
import Control.Monad (void)

import Debug.Trace

data LuaExpr = LuaNumber Int
             | LuaBool Bool
             | LuaVariable String
             | LuaString String
             | LuaArray [LuaExpr]
             | LuaArithOp String LuaExpr LuaExpr
             | LuaVarAssign String LuaExpr
             | LuaCallFunction String [LuaExpr]
             | LuaFunction String [LuaExpr]
             | LuaEnd Bool
             | LuaComparator String LuaExpr LuaExpr
             | LuaCond String LuaExpr LuaExpr
             | LuaWhile LuaExpr LuaExpr
             | LuaFor String LuaExpr LuaExpr LuaExpr
             | LuaAnd LuaExpr LuaExpr
             | LuaOr LuaExpr LuaExpr
             | LuaReturn LuaExpr
             deriving (Eq, Show)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Just (result, rest) -> Just (f result, rest)
            Nothing -> Nothing

instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    (Parser p1) <*> (Parser p2) = Parser $ \input ->
        case p1 input of
            Just (f, rest) -> case p2 rest of
                Just (a, finalRest) -> Just (f a, finalRest)
                Nothing -> Nothing
            Nothing -> Nothing

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input ->
        case p input of
            Just (result, rest) -> runParser (f result) rest
            Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Nothing -> p2 input
            result  -> result

-- Parsers basiques
parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
    case input of
        (x:xs) | x == c -> Just (c, xs)
        _               -> Nothing

parseDigits :: Parser String
parseDigits = Parser $ \input ->
    let (digits, rest) = span isDigit input
    in if null digits then Nothing else Just (digits, rest)

parseUInt :: Parser Int
parseUInt = read <$> parseDigits

parseInt :: Parser Int
parseInt = negativeParser <|> parseUInt
    where
        negativeParser = negate <$> (parseChar '-' *> parseUInt)

-- Parsers combi
parseOr :: Parser a -> Parser a -> Parser a
parseOr = (<|>)

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd (Parser p1) (Parser p2) = Parser $ \input ->
    case p1 input of
        Just (result1, rest1) -> case p2 rest1 of
            Just (result2, rest2) -> Just ((result1, result2), rest2)
            Nothing               -> Nothing
        Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser $ \input -> 
    case runParser (parseAnd p1 p2) input of
        Just ((result1, result2), rest) -> Just (f result1 result2, rest)
        Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \input ->
    case runParser p input of
        Just (result, rest) ->
            case runParser (parseMany p) rest of
                Just (results, finalRest) -> Just (result : results, finalRest)
        Nothing -> Just ([], input)

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p
parseSpace :: Parser ()
parseSpace = void $ many $ parseChar ' '

-- Parser pour les expressions Lua
parseLuaNumber :: Parser LuaExpr
parseLuaNumber = LuaNumber . read <$> parseDigits

parseLuaArithOp :: Parser LuaExpr
parseLuaArithOp = do
    parseChar '('
    parseSpace
    expr1 <- parseLuaExpr
    parseSpace
    op <- parseAnyChar "+-*/"
    parseSpace
    expr2 <- parseLuaExpr
    parseSpace
    parseChar ')'
    return $ LuaArithOp [op] expr1 expr2

parseLuaVarAssign :: Parser LuaExpr
parseLuaVarAssign = do
    string "local"
    parseSpace
    varName <- some $ parseAnyChar ['a'..'z']
    parseSpace
    parseChar '='
    parseSpace
    expr <- parseLuaExpr
    return $ LuaVarAssign varName expr

string :: String -> Parser String
string = traverse parseChar

parseAnyChar :: String -> Parser Char
parseAnyChar cs = Parser $ \input ->
    case input of
        (x:xs) | x `elem` cs -> Just (x, xs)
        _ -> Nothing

parseLuaString :: Parser LuaExpr
parseLuaString = do
    parseChar '\"'
    str <- parseMany (parseAnyCharBut "\"")
    parseChar '\"'
    return $ LuaString str

parseLuaBool :: Parser LuaExpr
parseLuaBool = (LuaBool True <$ string "true") <|> (LuaBool False <$ string "false")

parseLuaArray :: Parser LuaExpr
parseLuaArray = do
    parseChar '{'
    parseSpace
    exprs <- parseLuaExpr `parseSeparatedBy` (parseChar ',' *> parseSpace)
    parseSpace
    parseChar '}'
    return $ LuaArray exprs

parseSeparatedBy :: Parser a -> Parser sep -> Parser [a]
parseSeparatedBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

parseAnyCharBut :: String -> Parser Char
parseAnyCharBut cs = Parser $ \input ->
    case input of
        (x:xs) | not (x `elem` cs) -> Just (x, xs)
        _ -> Nothing

parseLuaVariable :: Parser LuaExpr
parseLuaVariable = LuaVariable <$> some (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ "_"))

parseLuaCallFunction :: Parser LuaExpr
parseLuaCallFunction = do
    functionName <- some (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ "_"))
    parseSpace
    parseChar '('
    parseSpace
    args <- parseLuaExpr `parseSeparatedBy` (parseChar ',' *> parseSpace)
    parseSpace
    parseChar ')'
    return $ LuaCallFunction functionName args

parseLuaFunction :: Parser LuaExpr
parseLuaFunction = do
    string "function"
    parseSpace
    functionName <- some (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ "_"))
    parseSpace
    parseChar '('
    params <- parseLuaVariable `parseSeparatedBy` (parseChar ',' *> parseSpace)
    parseChar ')'
    return $ LuaFunction functionName params

parseLuaEnd :: Parser LuaExpr
parseLuaEnd = LuaEnd True <$ string "end"

parseLuaComparator :: Parser LuaExpr
parseLuaComparator = do
    expr1 <- parseLuaExprWithoutComparator
    parseSpace
    compOp <- parseComparator
    parseSpace
    expr2 <- parseLuaExpr
    return $ LuaComparator compOp expr1 expr2

parseComparator :: Parser String
parseComparator = string "==" <|> string "<" <|> string ">" <|> string "<=" <|> string ">=" <|> string "!="

parseLuaCond :: Parser LuaExpr
parseLuaCond = do
    condType <- string "if" <|> string "else" <|> string "elseif"
    parseSpace
    condition <- if condType == "else" then return (LuaString "") else parseLuaExpr
    return $ LuaCond condType condition (LuaString "")

parseLuaWhile :: Parser LuaExpr
parseLuaWhile = do
    string "while"
    parseSpace
    condition <- parseLuaExpr
    return $ LuaWhile condition (LuaString "")

parseLuaFor :: Parser LuaExpr
parseLuaFor = do
    string "for"
    parseSpace
    varExpr <- parseLuaVariable
    case varExpr of
        LuaVariable varName -> do
            parseSpace
            parseChar '='
            parseSpace
            startExpr <- parseLuaExpr
            parseChar ','
            parseSpace
            endExpr <- parseLuaExpr
            stepExpr <- (parseChar ',' *> parseSpace *> parseLuaExpr) <|> pure (LuaNumber 1)
            return $ LuaFor varName startExpr endExpr stepExpr
        _ -> empty


parseLuaAnd :: Parser LuaExpr
parseLuaAnd = do
    expr1 <- parseLuaExprWithoutLogic
    parseSpace
    string "and"
    parseSpace
    expr2 <- parseLuaExpr
    return $ LuaAnd expr1 expr2

parseLuaOr :: Parser LuaExpr
parseLuaOr = do
    expr1 <- parseLuaExprWithoutLogic
    parseSpace
    string "or"
    parseSpace
    expr2 <- parseLuaExpr
    return $ LuaOr expr1 expr2

parseLuaReturn :: Parser LuaExpr
parseLuaReturn = do
    string "return"
    parseSpace
    expr <- parseLuaExpr
    return $ LuaReturn expr

parseLuaExpr :: Parser LuaExpr
parseLuaExpr = parseLuaCond <|> parseLuaComparator <|> parseLuaExprWithoutComparator

parseLuaExprWithoutLogic :: Parser LuaExpr
parseLuaExprWithoutLogic = parseLuaReturn <|> parseLuaWhile <|> parseLuaFor <|> parseLuaVarAssign <|> parseLuaArithOp <|> parseLuaNumber <|> 
                           parseLuaBool <|> parseLuaString <|> parseLuaArray <|>
                           parseLuaCallFunction <|> parseLuaVariable <|> parseLuaFunction <|>
                           parseLuaEnd

parseLuaExprWithoutComparator :: Parser LuaExpr
parseLuaExprWithoutComparator = parseLuaReturn <|> parseLuaAnd <|> parseLuaOr <|> parseLuaWhile <|> parseLuaFor <|> parseLuaVarAssign <|> parseLuaArithOp <|> parseLuaNumber <|> 
                                parseLuaBool <|> parseLuaString <|> parseLuaArray <|> 
                                parseLuaCallFunction <|> parseLuaVariable <|> parseLuaFunction <|>
                                parseLuaEnd

-- main :: IO ()
-- main = do
--     let testStrings = [ "local x = 2" -- LuaVarAssign "x" (LuaNumber 2)
--                         , "local y = (8 + 9)" -- LuaVarAssign "y" (LuaArithOp "+" (LuaNumber 8) (LuaNumber 9))
--                         , "local z = (2 * (3 + 5))" -- LuaVarAssign "z" (LuaArithOp "*" (LuaNumber 2) (LuaArithOp "+" (LuaNumber 3) (LuaNumber 5)))
--                         , "local a = true" -- LuaVarAssign "a" (LuaBool true)
--                         , "local b = false"  -- LuaVarAssign "b" (LuaBool false)
--                         , "local c = \"hello\"" -- LuaVarAssing "c" LuaString "hello"
--                         , "local d = {1, \"texte\", true, {1, 2, 3}, a}" -- LuaVarAssign "d" (LuaArray [LuaNumber 1, LuaString "text", LuaBool True, LuaArray [LuaNumber 1, LuaNumber 2, LuaNumber 3]])
--                         , "print(\"hello\")" -- LuaCallFunction print LuaString "hello"
--                         , "print(str)" -- LuaCallFunction print LuaVariable "str"
--                         , "sum(1, 2, 3)" -- LuaCallFunction "sum" [LuaNumber 1, LuaNumber 2, LuaNumber 3]
--                         , "local w = m" -- LuaVarAssign "w" (LuaVariale "m")
--                         , "local v = (1 + c)" -- LuaVarAssign "v" (LuaArithOp "+" (LuaNumber 1) (LuaVariable c))
--                         , "function addition(a, b)" -- LuaFuction "addition" [luaVariable "a", LuaVariable "b"]
--                         , "end" -- True
--                         , "20 == a" -- LuaComparator "==" LuaNumber 20 LuaVariable "a"
--                         , "18 < (19 + 12)" --LuaComparator "<" LuaNumber 18 LuaArithOp "+" (LuaNumber 19) (LuaNumber 12)
--                         , "if (a == b)" -- LuaCond "if" LuaComparator "==" LuaVariable "a" LuaVariable "b"
--                         , "else" -- LuaCond "else" "" ""
--                         , "while (condition)" -- LuaWhile (condition) (LuaString "")
--                         , "for i = 1,10,2" -- LuaFor "i" (LuaNumber 1) (LuaNumber 10) (LuaNumber 2)
--                         , "x or y" -- LuaOr (LuaVariable "x") (LuaVariable "y")
--                         , "return z" -- LuaReturn (LuaVariable "z")
--                       -- Autres exemples de test
--                       ]

--     putStrLn "Parsing Results:"
--     mapM_ (\input -> do
--               let result = runParser parseLuaExpr input
--               putStrLn $ "Input: " ++ input
--               putStrLn $ "Parsed: " ++ show result
--               putStrLn "") testStrings
