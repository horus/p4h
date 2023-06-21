module Main (main) where

import Lib (run)
import System.Console.ANSI
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run >>= either (colored Red) (colored Green)
  where
    colored clr txt = do
      setSGR [SetColor Foreground Vivid clr]
      putStr txt
      setSGR [Reset]
