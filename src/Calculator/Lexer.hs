module Calculator.Lexer
(
  tokenize,
  Token(..),
  Operator(..)
) where

import Data.Char

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokIdent String
           | TokNum Double
           | TokEnd
    deriving (Show, Eq)

opToStr :: Operator -> String
opToStr Plus  = "+"
opToStr Minus = "-"
opToStr Times = "*"
opToStr Div   = "/"

operator :: Char -> Operator
operator c
  | c == '+' = Plus
  | c == '-' = Minus
  | c == '*' = Times
  | c == '/' = Div

identifier :: Char -> String -> [Token]
identifier c cs =
  let (str, cs') = span isAlphaNum cs
  in  TokIdent (c:str) : tokenize cs'

number :: Char -> String -> [Token]
number c cs =
   let (digs, cs') = span isDigit cs
   in  TokNum (read (c : digs)) : tokenize cs'

tokenize ::  String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | c `elem` "+-*/" = TokOp (operator c) : tokenize cs
  | c == '=' = TokAssign : tokenize cs
  | c == '(' = TokLParen: tokenize cs
  | c == ')' = TokRParen: tokenize cs
  | isAlpha c = identifier c cs
  | isDigit c = number c cs
  | isSpace c = tokenize cs
  | otherwise = error $ "Cannot tokenize " ++ [c]