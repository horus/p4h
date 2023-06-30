module Main (main) where

import P4 (runP4)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runP4
