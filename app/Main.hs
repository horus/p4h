module Main (main) where

import Lib (runP4)
import System.Console.ANSI
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runP4 >>= either (colored Red) (colored Green)
  where
    colored clr txt = do
      setSGR [SetColor Foreground Vivid clr]
      putStr txt
      setSGR [Reset]
