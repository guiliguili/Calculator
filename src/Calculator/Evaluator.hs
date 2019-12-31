module Calculator.Evaluator
(
  Evaluator(..),
  evaluate
) where

import qualified Data.Map as M

import Calculator.Lexer
import Calculator.Parser

type SymTab = M.Map String Double

newtype Evaluator a = Ev (Either String a)

instance (Show a) => Show (Evaluator a) where
  show (Ev (Left msg)) = concat ["Ev(Left", " ", msg, ")"]
  show (Ev (Right v))  = concat ["Ev(Right", " ", show v, ")"]

instance Functor Evaluator where
  fmap f (Ev (Left msg)) = Ev (Left msg)
  fmap f (Ev (Right v))  = Ev (Right (f v))

instance Applicative Evaluator where
  pure v = Ev (Right v)

  Ev (Left msg) <*> _  = Ev (Left msg)
  Ev (Right f)  <*> m  = fmap f m

instance Monad Evaluator where
  Ev (Left msg) >>= _ = Ev (Left msg)
  Ev (Right v)  >>= k = k v

  fail msg = Ev (Left msg)

lookUp :: String -> SymTab -> Evaluator (Double, SymTab)
lookUp str symTab =
  case M.lookup str symTab of
    Just v  -> return (v, symTab)
    Nothing -> fail ("Undefined variable " ++ str)

addSymbol :: String -> Double -> SymTab -> Evaluator ((), SymTab)
addSymbol str val symTab =
  let symTab' = M.insert str val symTab
  in return ((), symTab')

evaluate :: Tree -> SymTab -> Evaluator (Double, SymTab)

evaluate (SumNode op left right) symTab =
  do
    (lft, symTab') <- evaluate left symTab
    (rgt, symTab'') <- evaluate right symTab'
    case op of
      Plus  -> return (lft + rgt, symTab'')
      Minus -> return (lft - rgt, symTab'')

evaluate (ProdNode op left right) symTab =
  do
    (lft, symTab') <- evaluate left symTab
    (rgt, symTab'') <- evaluate right symTab'
    case op of
      Times -> return (lft * rgt, symTab)
      Div   -> return (lft / rgt, symTab)

evaluate (UnaryNode op tree) symTab =
  do
    (x, symTab') <- evaluate tree symTab
    case op of
      Plus  -> return (x, symTab')
      Minus -> return (-x, symTab')

evaluate (NumNode x) symTab = return (x, symTab)

evaluate (VarNode str) symTab = lookUp str symTab

evaluate (AssignNode str tree) symTab =
  do
    (v, symTab')  <- evaluate tree symTab
    (_, symTab'') <- addSymbol str v symTab'
    return (v, symTab'')