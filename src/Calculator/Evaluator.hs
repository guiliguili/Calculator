module Calculator.Evaluator
(
  Evaluator(..),
  evaluate
) where

import qualified Data.Map as M

import Calculator.Lexer
import Calculator.Parser

type SymTab = M.Map String Double

newtype Evaluator a = Ev (SymTab -> (a, SymTab))

instance Functor Evaluator where
  fmap f (Ev act) = Ev $
    \symtab ->
      let (x, symtab') = act symtab
      in (f x, symtab')

instance Applicative Evaluator where
  pure v = Ev $ \symtab -> (v, symtab)
  Ev f <*> ev  = Ev $
    \symtab ->
      let (f', symtab') = f symtab
          Ev act = fmap f' ev
      in act symtab'

instance Monad Evaluator where
  (Ev act) >>= k = Ev $
    \symtab ->
        let (x, symtab') = act symtab
            (Ev act') = k x
        in act' symtab'
  return x = Ev $ \symtab -> (x, symtab)

lookUp :: String -> Evaluator Double
lookUp str = Ev $ \symTab ->
  case M.lookup str symTab of
    Just v  -> (v, symTab)
    Nothing -> error $ "Undefined variable " ++ str

addSymbol :: String -> Double -> Evaluator Double
addSymbol str val = Ev $ \symTab ->
  let symTab' = M.insert str val symTab
  in (val, symTab')

evaluate :: Tree -> Evaluator Double

evaluate (SumNode op left right) = do
  lft <- evaluate left
  rgt <- evaluate right
  case op of
    Plus  -> return $ lft + rgt
    Minus -> return $ lft - rgt

evaluate (ProdNode op left right) = do
  lft <- evaluate left
  rgt <- evaluate right
  case op of
    Times -> return $ lft * rgt
    Div   -> return $ lft / rgt

evaluate (UnaryNode op tree) = do
  x <- evaluate tree
  case op of
    Plus  -> return x
    Minus -> return (-x)

evaluate (NumNode x) = return x

evaluate (VarNode str) = lookUp str

evaluate (AssignNode str tree) = do
  x <- evaluate tree
  addSymbol str x