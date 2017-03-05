module Calculator.Lexer
(
  Token(..),
  showContent,
  operator,
  tokenize
) where

import Data.Char

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Token = TokOp Operator
              | TokIdent String
              | TokNum Int
               deriving (Show, Eq)
-- show
showContent :: Token -> String
showContent (TokOp op) = opToStr op
showContent (TokIdent str) = str
showContent (TokNum i) = show i

opToStr :: Operator -> String
opToStr Plus  = "+"
opToStr Minus = "-"
opToStr Times = "*"
opToStr Div   = "/"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) | elem c "+-*/" = TokOp (operator c) : tokenize cs
                  | isDigit c  = TokNum (digitToInt c) : tokenize cs
                  | isAlpha c  = TokIdent [c]          : tokenize cs
                  | isSpace c  = tokenize cs
                  | otherwise  = error $ "Cannot tokenize " ++ [c]
