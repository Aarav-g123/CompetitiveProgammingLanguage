module Transpiler (transpileToCpp) where

type AST = [String] -- Placeholder for Abstract Syntax Tree. Match with Parser's definition.

-- Transpile AST to C++ code
transpileToCpp :: AST -> String
transpileToCpp ast = 
    unlines $ "#include <iostream>" : 
    "using namespace std;" : "" : 
    map transpileLine ast

-- Dummy transpiler (convert 1-to-1 with input lines)
transpileLine :: String -> String
transpileLine line = "// Transpiled line: " ++ line