module Language.Pretty
  ( prettyPrint
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.AST

prettyPrint :: Program -> Text
prettyPrint = T.pack . show  -- Basic implementation for now