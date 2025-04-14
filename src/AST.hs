{-
-- EPITECH PROJECT, 2023
-- AST.hs
-- File description:
-- Abstract Syntax Tree
-}

module AST where

import qualified Data.Map as Map
import Data.Function(on)

data SExpr = SInt Int
           | SSym String
           | SList [SExpr]
           deriving (Eq, Show, Ord)

data Ast = Define { varName :: String, varExpr :: Ast }
         | AstInt Int
         | AstSym String
         | AstBool Bool
         | Call { funcName :: Ast, args :: [Ast] }
         deriving (Eq, Show)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInt x)    = Just (AstInt x)
sexprToAST (SSym s)    = Just (AstSym s)
sexprToAST (SList [])  = Nothing
sexprToAST (SList (x:xs)) = case x of
    SSym "define" -> case xs of
        [SSym varName, expr] -> Define varName <$> sexprToAST expr
        _                    -> Nothing
    _ -> Call <$> sexprToAST x <*> traverse sexprToAST xs

evalAST :: Ast -> Maybe Ast
evalAST (AstInt n) = Just (AstInt n) 
evalAST (AstSym s) = Nothing
evalAST (Call (AstSym "+") args) = sumArgs =<< traverse evalAST args
evalAST (Call (AstSym "*") args) = productArgs =<< traverse evalAST args
evalAST (Call (AstSym "-") args) = subArgs =<< traverse evalAST args
evalAST (Call (AstSym "/") args) = divArgs =<< traverse evalAST args
evalAST _ = Nothing

sumArgs :: [Ast] -> Maybe Ast
sumArgs args = fmap (AstInt . sum) (mapM evalInt args)

subArgs :: [Ast] -> Maybe Ast
subArgs [] = Nothing
subArgs [x] = Just x
subArgs (x:xs) = case subArgs xs of
    Just result -> case (evalInt x, evalInt result) of
        (Just n, Just m) -> Just (AstInt (n - m))
        _ -> Nothing
    Nothing -> Nothing

divArgs :: [Ast] -> Maybe Ast
divArgs [] = Nothing
divArgs [x] = Just x
divArgs (x:xs) = case divArgs xs of
    Just result -> case (evalInt x, evalInt result) of
        (_, Just 0) -> Nothing  -- Division par zéro
        (Just n, Just m) -> Just (AstInt (n `div` m))
        _ -> Nothing
    Nothing -> Nothing

productArgs :: [Ast] -> Maybe Ast
productArgs args = fmap (AstInt . product) (mapM evalInt args)

evalInt :: Ast -> Maybe Int
evalInt (AstInt n) = Just n
evalInt _ = Nothing

tableauAssociatif :: Map.Map AST.SExpr AST.SExpr
tableauAssociatif = Map.fromList [(AST.SSym "a", AST.SSym "1"), (AST.SSym "b", AST.SInt 1), (AST.SSym "a", AST.SSym "toto")]

isString :: AST.SExpr -> Bool
isString (AST.SSym _) = True
isString _ = False

doDefine :: Map.Map AST.SExpr AST.SExpr -> AST.SExpr -> AST.SExpr -> Maybe (Map.Map AST.SExpr AST.SExpr)
doDefine myMap expr1 expr2 = case expr1 of
    AST.SSym sym -> if Map.member expr1 myMap
                        then error "La clef existe deja"
                        else Just $ Map.insert expr1 expr2 myMap
    _        -> error "La première SExpr n'est pas une SSym : Arrêt du programme."

-- main :: IO ()
-- main = do
--     print $ evalAST (Call (AstSym "+") [AstInt 2, AstInt 3]) -- Doit retourner Just (AstInt 5)
--     print $ evalAST (Call (AstSym "*") [AstInt 4, AstInt 5]) -- Doit retourner Just (AstInt 20)
--     print $ evalAST (Call (AstSym "/") [AstInt 10, AstInt 2]) -- Doit retourner Just (AstInt 20)
--     print $ evalAST (Call (AstSym "*") [Call (AstSym "+") [AstInt 4, AstInt 3], AstInt 6]) -- Doit retourner Just (AstInt 42)
--     print $ evalAST (Call (AstSym "-") [AstInt 6, Call (AstSym "*") [AstInt 4, AstInt 3]]) -- Doit retourner Just (AstInt 6)
--     print $ evalAST (AstSym "non_supported") -- Doit retourner Nothing
--     let oto = SList [SSym "*", SInt 42, SInt 5]
--     let oui = sexprToAST oto
--     case oui of
--         Just ast -> print $ evalAST ast  -- Si oui est Just ast, évaluez-le
--         Nothing -> putStrLn "Invalid S-expression" 

-- main pour tester la map
-- main :: IO ()
-- main = do
--     let maMap = Map.fromList [(AST.SSym "x", AST.SSym "1"), (AST.SSym "y", AST.SInt 1), (AST.SSym "z", AST.SSym "toto")]
--         expr1 = AST.SSym "example"
--         expr2 = AST.SInt 42
--         result = doDefine maMap expr1 expr2
--     case result of
--         Just newMap -> putStrLn $ "La nouvelle map est : " ++ show newMap
--         Nothing -> putStrLn "Erreur : La clé existe déjà ou la première SExpr n'est pas une SSym." 
