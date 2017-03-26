module Main where

import Calculator.Lexer

main :: IO ()
main = do
  str <- getLine
  do
    if null str
     then
        return ()
     else
        do
          print $ tokenize str
          main