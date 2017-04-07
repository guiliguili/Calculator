module Main where

import qualified Data.Map as M

import Calculator.Lexer
import Calculator.Parser
import Calculator.Evaluator

main :: IO ()
main = do
  loop (M.fromList [("pi", pi)])

loop symTab = do
  str <- getLine
  if null str
  then
    return ()
  else
    let
      toks = tokenize str
      tree = parse toks
      (val, symTab') = evaluate tree symTab
    in do
      print val
      loop symTab'