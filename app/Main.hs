module Main where

import Lib (transpileFile, transpileCode)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    ["-h"] -> printHelp
    ["--help"] -> printHelp
    ["-v"] -> printVersion
    ["--version"] -> printVersion
    [inputFile] -> transpileToFile inputFile (replaceExtension inputFile ".cpp")
    [inputFile, outputFile] -> transpileToFile inputFile outputFile
    _ -> do
      putStrLn "Usage: cp-to-cpp [input.cp] [output.cpp]"
      putStrLn "       cp-to-cpp --help"

-- | Transpile input file to output file
transpileToFile :: FilePath -> FilePath -> IO ()
transpileToFile input output = do
  exists <- doesFileExist input
  if not exists
    then putStrLn $ "Error: File not found: " ++ input
    else do
      result <- transpileFile input
      case result of
        Left err -> do
          putStrLn "Transpilation Error:"
          putStrLn err
        Right cppCode -> do
          writeFile output cppCode
          putStrLn $ "Successfully transpiled " ++ input ++ " to " ++ output
          putStrLn $ "Compile with: g++ -O2 -std=c++17 " ++ output ++ " -o solution"

-- | Interactive REPL mode
runRepl ::  IO ()
runRepl = do
  putStrLn "CP Language REPL - Enter expressions or statements"
  putStrLn "Type : quit to exit, :help for help"
  replLoop

replLoop :: IO ()
replLoop = do
  putStr "cp> "
  input <- getLine
  case input of
    ":quit" -> putStrLn "Goodbye!"
    ": q" -> putStrLn "Goodbye!"
    ":help" -> do
      putStrLn "Commands:"
      putStrLn "  : quit, :q  - Exit the REPL"
      putStrLn "  :help      - Show this help"
      putStrLn ""
      putStrLn "Enter any CP expression to see its C++ translation."
      replLoop
    "" -> replLoop
    _ -> do
      case transpileCode input of
        Left err -> putStrLn $ "Error: " ++ err
        Right result -> do
          putStrLn "C++ Output:"
          putStrLn result
      replLoop

-- | Print help message
printHelp ::  IO ()
printHelp = do
  putStrLn "CP Language - A Python-like language for competitive programming"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  cp-to-cpp                    Start interactive REPL"
  putStrLn "  cp-to-cpp <input.cp>         Transpile to <input.cpp>"
  putStrLn "  cp-to-cpp <input> <output>   Transpile to custom output file"
  putStrLn "  cp-to-cpp --help             Show this help"
  putStrLn "  cp-to-cpp --version          Show version"
  putStrLn ""
  putStrLn "Example:"
  putStrLn "  cp-to-cpp solution.cp solution.cpp"
  putStrLn "  g++ -O2 -std=c++17 solution.cpp -o solution"

-- | Print version
printVersion :: IO ()
printVersion = putStrLn "CP Language v0.1.0"