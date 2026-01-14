{-# LANGUAGE OverloadedStrings #-}

module Language.Builtins
  ( transpileBuiltin
  , builtinFunctions
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)

-- | List of built-in functions with their C++ equivalents
builtinFunctions ::  [(Text, Text)]
builtinFunctions =
  -- Math functions
  [ ("abs", "abs")
  , ("min", "min")
  , ("max", "max")
  , ("pow", "pow")
  , ("sqrt", "sqrt")
  , ("log", "log")
  , ("log2", "log2")
  , ("log10", "log10")
  , ("ceil", "ceil")
  , ("floor", "floor")
  , ("round", "round")
  , ("gcd", "__gcd")
  , ("lcm", "lcm")
  
  -- Container functions
  , ("len", "sz")
  , ("size", "sz")
  , ("empty", "empty")
  , ("push", "push_back")
  , ("pop", "pop_back")
  , ("front", "front")
  , ("back", "back")
  , ("clear", "clear")
  , ("insert", "insert")
  , ("erase", "erase")
  , ("find", "find")
  , ("count", "count")
  
  -- Sorting and searching
  , ("sort", "sort")
  , ("reverse", "reverse")
  , ("unique", "unique")
  , ("lower_bound", "lower_bound")
  , ("upper_bound", "upper_bound")
  , ("binary_search", "binary_search")
  , ("next_permutation", "next_permutation")
  , ("prev_permutation", "prev_permutation")
  
  -- String functions
  , ("str", "to_string")
  , ("int", "stoi")
  , ("int64", "stoll")
  , ("float", "stof")
  , ("double", "stod")
  , ("substr", "substr")
  , ("to_lower", "tolower")
  , ("to_upper", "toupper")
  
  -- I/O
  , ("input", "cin >>")
  , ("output", "cout <<")
  ]

-- | Transpile a built-in function call
transpileBuiltin :: Text -> [String] -> String
transpileBuiltin name args = case T.unpack name of
  -- Special cases that need custom handling
  "len" -> "sz(" ++ head args ++ ")"
  "size" -> "sz(" ++ head args ++ ")"
  
  "sort" -> case args of
    [arr] -> "sort(all(" ++ arr ++ "))"
    [arr, cmp] -> "sort(all(" ++ arr ++ "), " ++ cmp ++ ")"
    _ -> "sort(" ++ intercalate ", " args ++ ")"
  
  "reverse" -> case args of
    [arr] -> "reverse(all(" ++ arr ++ "))"
    _ -> "reverse(" ++ intercalate ", " args ++ ")"
  
  "min" -> case args of
    [a, b] -> "min(" ++ a ++ ", " ++ b ++ ")"
    _ -> "min({" ++ intercalate ", " args ++ "})"
  
  "max" -> case args of
    [a, b] -> "max(" ++ a ++ ", " ++ b ++ ")"
    _ -> "max({" ++ intercalate ", " args ++ "})"
  
  "gcd" -> "__gcd(" ++ intercalate ", " args ++ ")"
  
  "lcm" -> let [a, b] = args in "(" ++ a ++ " / __gcd(" ++ a ++ ", " ++ b ++ ") * " ++ b ++ ")"
  
  "pow" -> case args of
    [base, exp] -> "pow(" ++ base ++ ", " ++ exp ++ ")"
    [base, exp, mod] -> "powmod(" ++ base ++ ", " ++ exp ++ ", " ++ mod ++ ")"
    _ -> "pow(" ++ intercalate ", " args ++ ")"
  
  "lower_bound" -> case args of
    [arr, val] -> "lower_bound(all(" ++ arr ++ "), " ++ val ++ ")"
    _ -> "lower_bound(" ++ intercalate ", " args ++ ")"
  
  "upper_bound" -> case args of
    [arr, val] -> "upper_bound(all(" ++ arr ++ "), " ++ val ++ ")"
    _ -> "upper_bound(" ++ intercalate ", " args ++ ")"
  
  "binary_search" -> case args of
    [arr, val] -> "binary_search(all(" ++ arr ++ "), " ++ val ++ ")"
    _ -> "binary_search(" ++ intercalate ", " args ++ ")"
  
  "unique" -> case args of
    [arr] -> arr ++ ".erase(unique(all(" ++ arr ++ ")), " ++ arr ++ ".end())"
    _ -> "unique(" ++ intercalate ", " args ++ ")"
  
  "sum" -> case args of
    [arr] -> "accumulate(all(" ++ arr ++ "), 0LL)"
    [arr, init] -> "accumulate(all(" ++ arr ++ "), " ++ init ++ ")"
    _ -> "accumulate(" ++ intercalate ", " args ++ ")"
  
  "count" -> case args of
    [arr, val] -> "count(all(" ++ arr ++ "), " ++ val ++ ")"
    _ -> "count(" ++ intercalate ", " args ++ ")"
  
  "fill" -> case args of
    [arr, val] -> "fill(all(" ++ arr ++ "), " ++ val ++ ")"
    _ -> "fill(" ++ intercalate ", " args ++ ")"
  
  "swap" -> "swap(" ++ intercalate ", " args ++ ")"
  
  "make_pair" -> "mp(" ++ intercalate ", " args ++ ")"
  
  "make_tuple" -> "make_tuple(" ++ intercalate ", " args ++ ")"
  
  "get" -> case args of
    [idx, tup] -> "get<" ++ idx ++ ">(" ++ tup ++ ")"
    _ -> "get(" ++ intercalate ", " args ++ ")"
  
  -- Graph/DP helpers
  "inf" -> "INF"
  "linf" -> "LINF"
  "mod" -> "MOD"
  
  -- Default:  just call the function as-is
  other -> other ++ "(" ++ intercalate ", " args ++ ")"