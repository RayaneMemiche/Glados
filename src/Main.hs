{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Main file
-}
module Main where

import System.Environment (getArgs)
import System.IO (
    readFile,
    hSetBinaryMode,
    stdin,
    hPutStr,
    IOMode (WriteMode),
    openFile, hClose
    )
import System.Console.GetOpt
import Data.List

import Parser (runParser, parseLuaExpr)
import VM
import Compiler (luaCompile)
import Data.Maybe (fromJust)
import System.Exit (exitSuccess)

data Options = Options
  { optVM :: Bool
  , optCompilo :: Bool
  , optHelp :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optVM = False
  , optCompilo = False
  , optHelp = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "vm" ["vm"] (NoArg (\opts -> opts { optVM = True }))
    "Exécute le code VM"
  , Option "compilo" ["compile"] (NoArg (\opts -> opts { optCompilo = True }))
    "Appelle le compilateur"
  , Option "h" ["help"] (NoArg (\opts -> opts { optHelp = True }))
    "Affiche ce message d'aide"
  ]

main :: IO ()
main = do
    hSetBinaryMode stdin True
    args <- getArgs
    let (opts, _, _) = getOpt Permute options args
    let options = foldl (flip id) defaultOptions opts
    if optHelp options then
        printHelp
    else
        case (optVM options, optCompilo options) of
            (True, False) -> runVM =<< getContents
            (False, True) -> runCompilo =<< getContents
            _ -> putStrLn "Veuillez spécifier une seule option: -vm ou -compile"

printHelp :: IO ()
printHelp = do
    putStrLn "Utilisation: <program> [OPTION]"
    putStrLn "Options:"
    putStr $ usageInfo "" options
    putStrLn "\n"
    exitSuccess

runVM :: String -> IO ()
runVM fileContent = case execLoop 0 fileContent [] [] (replicate 4096 0) of
        Left num -> print num
        Right str -> putStrLn str

runCompilo :: String -> IO ()
runCompilo fileContent = do
    outFile <- openFile "out.bin" WriteMode
    hSetBinaryMode outFile True
    hPutStr outFile (
        map (toEnum . fromEnum) bytecodes)
    hClose outFile
    where asts =
            map (
                luaCompile .
                fst . 
                fromJust .
                runParser parseLuaExpr
            ) (lines fileContent)
          bytecodes = concat asts
