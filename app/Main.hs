module Main where

import qualified Data.Map as M

import Calculator.Lexer
import Calculator.Parser
import Calculator.Evaluator

main :: IO ()
main = loop (M.fromList [("pi", pi)])

loop symTab = do
  str <- getLine
  if null str
  then
    return ()
  else
    let
      toks = tokenize str
      tree = parse toks
      Ev act = evaluate tree
      (val, symTab') = act symTab
    in do
      print val
      loop symTab'