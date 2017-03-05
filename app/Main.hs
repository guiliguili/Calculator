module Main where

import Calculator.Lexer

main :: IO ()
main = print $ tokenize " 1 + 4 / x "
