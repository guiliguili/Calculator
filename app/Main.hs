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
    in
      case evaluate tree symTab of
        Ev (Left msg) -> do
          putStrLn $ "Error: " ++ msg
          loop symTab
        Ev (Right (v, symTab')) -> do
          print v
          loop symTab