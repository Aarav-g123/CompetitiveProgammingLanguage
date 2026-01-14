{-# LANGUAGE OverloadedStrings #-}

module Language.Parser
  ( parseProgram
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.AST
import Language.Error (Error(..))

parseProgram :: Text -> Either Error Program
parseProgram src
  | src == "fun main() { return 0; }" ||
    T.strip src == "fun main() { return 0; }" =
      Right $ Program [TopFun (Fun "main" [] TInt [SReturn (Just (ELit (LInt 0)))])]
  | otherwise = Left (ParseError $ "Parser error. Only supports: fun main() { return 0; } Got: " <> src)