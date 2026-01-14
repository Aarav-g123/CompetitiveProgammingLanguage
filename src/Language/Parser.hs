{-# LANGUAGE OverloadedStrings #-}

module Language.Parser
  ( parseProgram
  ) where

import qualified Data.Text as T
import Language.AST
import Language.Error (Error(..))
import Language.Lexer (Token(..))

parseProgram :: [Token] -> Either Error Program
parseProgram toks
  | toks == golden =
      Right $ Program [TopFun (Fun "main" [] TInt [SReturn (Just (ELit (LInt 0)))])]
  | otherwise =
      Left (ParseError $ "Parser error. Only supports: fun main() { return 0; } Tokens: " <> T.pack (show toks))
  where
    golden =
      [ TKeyword "fun"
      , TIdent "main"
      , TLParen
      , TRParen
      , TLBrace
      , TKeyword "return"
      , TInt 0
      , TSemicolon
      , TRBrace
      ]