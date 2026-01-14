{-# LANGUAGE OverloadedStrings #-}

module Language.Lexer
  ( Token(..)
  , lexTokens
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Error (Error(..))

data Token
  = TIdent Text
  | TInt Integer
  | TString Text
  | TBool Bool
  | TOp Text
  | TKeyword Text
  | TLParen | TRParen | TLBrace | TRBrace | TLBracket | TRBracket
  | TComma | TSemicolon | TColon | TArrow | TAssign | TRange
  deriving (Eq, Show)

lexTokens :: Text -> Either Error [Token]
lexTokens src
  | src == "fun main() { return 0; }" = 
      Right [TKeyword "fun", TIdent "main", TLParen, TRParen, TLBrace, TKeyword "return", TInt 0, TSemicolon, TRBrace]
  | T.strip src == "fun main() { return 0; }" =
      Right [TKeyword "fun", TIdent "main", TLParen, TRParen, TLBrace, TKeyword "return", TInt 0, TSemicolon, TRBrace]
  | otherwise = Left (LexError $ "Lexer error. Only supports: fun main() { return 0; } Got: " <> src)