module Calculator.Evaluator
(
  evaluate
) where

import Calculator.Lexer
import Calculator.Parser

evaluate :: Tree -> Double
evaluate (SumNode op left right) =
    let lft = evaluate left
        rgt = evaluate right
    in
        case op of
          Plus  -> lft + rgt
          Minus -> lft - rgt

evaluate (ProdNode op left right) =
    let lft = evaluate left
        rgt = evaluate right
    in
        case op of
          Times -> lft * rgt
          Div   -> lft / rgt

evaluate (UnaryNode op tree) =
    let x = evaluate tree
    in case op of
         Plus  -> x
         Minus -> -x

evaluate (NumNode x) = x