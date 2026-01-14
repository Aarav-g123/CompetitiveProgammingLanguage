module Lib
  ( module Language.AST
  , module Language.Lexer
  , module Language.Parser
  , module Language.Transpiler
  , module Language.Builtins
  , transpileFile
  , transpileCode
  ) where

import Language.AST
import Language.Lexer
import Language.Parser
import Language.Transpiler
import Language.Builtins

import qualified Data.Text as T

-- | Transpile CP source code to C++
transpileCode :: String -> Either String String
transpileCode source = do
  ast <- parseCP (T.pack source)
  return $ transpileToCpp ast

-- | Transpile a file path to C++ code
transpileFile ::  String -> IO (Either String String)
transpileFile path = do
  content <- readFile path
  return $ transpileCode content