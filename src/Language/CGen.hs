{-# LANGUAGE OverloadedStrings #-}

module Language.CGen
  ( emitC
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.IR

emitC :: IRProgram -> Text
emitC (IRProgram funs) = T.unlines $ map emitFun funs

emitFun :: IRFun -> Text
emitFun (IRFun name args body) = T.unlines $
  [ "int " <> name <> "(" <> T.intercalate ", " (map (\a -> "int " <> a) args) <> ") {"
  ] ++ map ("  " <>) (map emitStmt body) ++
  [ "}" ]

emitStmt :: IRStmt -> Text
emitStmt (IRReturn (Just (IRLitInt n))) = "return " <> T.pack (show n) <> ";"
emitStmt (IRReturn (Just (IRVar v))) = "return " <> v <> ";"
emitStmt (IRReturn Nothing) = "return;"
emitStmt (IRAssign var expr) = var <> " = " <> emitExpr expr <> ";"
emitStmt (IRIf cond thenStmts elseStmts) = T.unlines $
  [ "if (" <> emitExpr cond <> ") {" ] ++
  map ("    " <>) (map emitStmt thenStmts) ++
  [ "} else {" ] ++
  map ("    " <>) (map emitStmt elseStmts) ++
  [ "}" ]
emitStmt (IRWhile cond body) = T.unlines $
  [ "while (" <> emitExpr cond <> ") {" ] ++
  map ("    " <>) (map emitStmt body) ++
  [ "}" ]

emitExpr :: IRExpr -> Text
emitExpr (IRVar name) = name
emitExpr (IRLitInt n) = T.pack (show n)
emitExpr (IRBin op e1 e2) = "(" <> emitExpr e1 <> " " <> op <> " " <> emitExpr e2 <> ")"
emitExpr (IRCall name args) = name <> "(" <> T.intercalate ", " (map emitExpr args) <> ")"