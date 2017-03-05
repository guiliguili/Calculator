module Main where

import Calculator.Lexer

token :: Token
token = TokIdent "x"

main = do
    putStrLn $ showContent token
    print token
