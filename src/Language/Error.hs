{-# LANGUAGE LambdaCase #-}

module Language.Error
  ( Error(..)
  , renderError
  ) where

import Data.Text (Text)
import qualified Data.Text as T

data Error
  = LexError Text
  | ParseError Text
  | TypeError Text
  | InternalError Text
  deriving (Eq, Show)

renderError :: Error -> String
renderError = \case
  LexError t      -> "Lex error: " <> T.unpack t
  ParseError t    -> "Parse error: " <> T.unpack t
  TypeError t     -> "Type error: " <> T.unpack t
  InternalError t -> "Internal error: " <> T.unpack t