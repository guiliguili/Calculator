module Main where

import Calculator.Lexer
import Calculator.Parser

main :: IO ()
main = do
  str <- getLine
  do
    if null str
     then
        return ()
     else
        do
          print $ parse $ tokenize str
          main