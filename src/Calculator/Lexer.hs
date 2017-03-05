module Calculator.Lexer
(
  Token(..),
  showContent,
  tokenize
) where

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

tokenize :: String -> [Token]
tokenize = undefined
