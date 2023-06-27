module Main (main) where

import Lib (runP4)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runP4
