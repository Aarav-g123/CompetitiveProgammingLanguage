module Language.Desugar
  ( desugar
  ) where

import Language.AST

desugar :: Program -> Program
desugar = id -- TODO: lower for/if sugar etc.