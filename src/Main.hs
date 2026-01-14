module Main where

import Parser (parseCP)
import Transpiler (transpileToCpp)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

-- Entry point of the program
main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> do
            content <- readFile inputFile
            case parseCP content of
                Left err -> hPutStrLn stderr $ "Error: Parsing failed\n" ++ err
                Right ast -> writeFile outputFile (transpileToCpp ast)
        _ -> hPutStrLn stderr "Usage: cp-to-cpp <inputFile.cp> <outputFile.cpp>"