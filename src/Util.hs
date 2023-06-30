module Util
  ( colored,
    Color (..),
    both,
  )
where

import System.Console.ANSI

colored :: Color -> String -> IO ()
colored clr txt = do
  setSGR [SetColor Foreground Vivid clr]
  putStr txt
  setSGR [Reset]

both :: (a -> IO b) -> (a, a) -> IO (b, b)
both m (a, b) = do
  a' <- m a
  b' <- m b
  return (a', b')
