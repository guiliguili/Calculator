module Calculator.Evaluator
(
  evaluate
) where

import qualified Data.Map as M

import Calculator.Lexer
import Calculator.Parser

type SymTab = M.Map String Double

evaluate :: Tree -> SymTab -> (Double, SymTab)
evaluate = undefined