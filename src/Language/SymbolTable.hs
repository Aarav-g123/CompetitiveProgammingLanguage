module Language.SymbolTable
  ( SymTable
  , emptyTable
  , insertSym
  , lookupSym
  ) where

import qualified Data.Map.Strict as M
import Language.AST

newtype SymTable = SymTable (M.Map Ident Type)
  deriving (Eq, Show)

emptyTable :: SymTable
emptyTable = SymTable M.empty

insertSym :: Ident -> Type -> SymTable -> SymTable
insertSym k v (SymTable m) = SymTable (M.insert k v m)

lookupSym :: Ident -> SymTable -> Maybe Type
lookupSym k (SymTable m) = M.lookup k m