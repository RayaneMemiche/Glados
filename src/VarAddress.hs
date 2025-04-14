module VarAddress where

import qualified Data.Map as Map
import System.Environment
import Control.Monad.State
import Data.List (words)
import Data.Char (intToDigit)

type Memory = Map.Map String Int

transformFile :: FilePath -> IO ()
transformFile filePath = do
    content <- readFile filePath
    let (transformedContent, _) = runState (mapM transformLine (lines content)) 0
    writeFile "output.txt" (unlines transformedContent)

transformLine :: String -> State Int String
transformLine line = do
    currentAddress <- get
    let wordsOfLine = words line
    case wordsOfLine of
        (instruction:variable:rest) -> do
            newVariable <- updateMemory instruction variable currentAddress
            modify (+1)  -- Met à jour la dernière adresse mémoire
            return $ unwords (instruction : show newVariable : rest)
        _ -> return line

updateMemory :: String -> String -> Int -> State Int Int
updateMemory instruction variable currentAddress = do
    case instruction of
        "CALL" -> updateVariable variable currentAddress
        "CALLP" -> updateVariable variable currentAddress
        "JR" -> updateVariable variable currentAddress
        "JRZ" -> updateVariable variable currentAddress
        "STORE" -> updateVariable variable currentAddress
        "LOAD" -> updateVariable variable currentAddress
        _ -> return (read variable)

updateVariable :: String -> Int -> State Int Int
updateVariable variable currentAddress = do
    newAddress <- get
    return $ if Map.member variable memory
             then memory Map.! variable
             else newAddress
  where
    memory = Map.singleton variable currentAddress

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> transformFile filePath
