module Main where

import Calculator.Lexer

main :: IO ()
main = print $ deSpace $ tokenize " 1 + 4 / x "