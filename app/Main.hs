module Main where

import Calculator.Lexer
import Calculator.Parser
import Calculator.Evaluator

main :: IO ()
main = do
  str <- getLine
  do
    if null str
     then
        return ()
     else
        do
          print $ evaluate $ parse $ tokenize str
          main