module Calculator.Lexer
(
  tokenize,
  Token,
  alnums
) where

import Data.Char

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Token = TokOp Operator
              | TokIdent String
              | TokNum Int
              | TokSpace
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

alnums :: String -> (String, String)
alnums str = als "" str
  where
    als acc [] = (acc, [])
    als acc (c : cs) | isAlphaNum c =
                           let (acc', cs') = als acc cs
                           in (c : acc', cs')
                     | otherwise = (acc, c : cs)


digits ::  String -> (String, String)
digits str = digs "" str
  where
    digs acc [] = (acc, [])
    digs acc (c : cs) | isDigit c =
                          let (acc', cs') = digs acc cs
                          in (c : acc', cs')
                       | otherwise = (acc, c : cs)

identifier c cs =
  let (str, cs') = alnums cs
  in TokIdent (c:str) : tokenize cs'

number c cs =
  let (digs, cs') = digits cs
  in TokNum (read (c : digs)) : tokenize cs'

tokenize ::  String -> [Token]
tokenize [] = []
tokenize (c : cs) | isAlpha c = identifier c cs
                  | isDigit c = number c cs