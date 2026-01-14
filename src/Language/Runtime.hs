{-# LANGUAGE OverloadedStrings #-}

module Language.Runtime
  ( runtimeC
  ) where

import Data.Text (Text)
import qualified Data.Text as T

runtimeC :: Text
runtimeC = T.unlines
  [ "/* Minimal runtime: fast IO, vectors, etc. TODO implement */"
  , "#include <stdint.h>"
  , "#include <stdio.h>"
  , ""
  ]