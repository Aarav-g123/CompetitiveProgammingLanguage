module Main where

import qualified Main as LibraryMain

-- Just a wrapper to redirect to library Main (`src/Main.hs`)
main :: IO ()
main = LibraryMain.main