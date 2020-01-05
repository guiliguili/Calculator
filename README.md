# haskell-calculator

## Overview
This demonstrates how to implement a simple Calculator with [Haskell][1].

It follows the [Basics of Haskell][2] tutorial by [Bartosz Milewski][7] to illustrate how a full blown Haskell project may look like through a concrete and yet simple example. 

## Building with Haskell Tool Stack

haskell-calculator uses the [Haskell Tool Stack][3] to setup, build, test and run the project.

## Running

### From Command Line

When you run the calculator from the command line, it will continuously prompt for an arithmetic expression, parse and evaluate it.
  
```bash
$ stack exec haskell-calculator-exe
x1 = -15 / (2 + pi)
-2.9173839723625705
```

### From REPL

You can also interactively test lower level function from GHCi as follow :

```bash
$ stack ghci
-- Run main
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> :main
x1 = -15 / (2 + pi)
-2.9173839723625705

-- Tokenize expression 
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> tokenize "x1 = -15 / (2 + pi)"
[TokNum 1.0,TokOp Plus,TokNum 2.0]

-- Parse tokens
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> parse $ tokenize "x1 = -15 / (2 + pi)"
SumNode Plus (NumNode 1.0) (NumNode 2.0)

-- Evaluate expression
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> let (Ev act) = evaluate (parse $ tokenize "x1 = -15 / (2 + pi)")
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> let (val, symTab) = act $ M.fromList [("pi", pi)]
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> val
-2.9173839723625705
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> symTab
fromList [("pi",3.141592653589793),("x1",-2.9173839723625705)]

-- Apply Functor
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> let (Ev actMultBy2) = fmap (*2) (Ev act)
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> let (val, symTab) = actMultBy2 $ M.fromList [("pi", pi)]
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> val
-5.834767944725141
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> symTab
fromList [("pi",3.141592653589793),("x1",-2.9173839723625705)]

-- Apply Applicative
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> let (Ev actMultBy10) = (pure ( *10 )) <*> (evaluate (parse $ tokenize "x1 = -15 / (2 + pi)"))
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> let (val, symTab) = actMultBy10 $ M.fromList [("pi", pi)]
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> val
-29.173839723625704
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> symTab 
fromList [("pi",3.141592653589793),("x1",-2.9173839723625705)]
```

## Testing

The calculator test suite is implemented using [Hspec][5] and located under the [test][4] folder. You run the suite as follow :

```bash
$ stack test
  haskell-calculator> test (suite: haskell-calculator-test)
  
  
  Calculator.Lexer
    tokenize
      tokenizing number
  
  Finished in 0.0004 seconds
  1 example, 0 failures
  
  haskell-calculator> Test suite haskell-calculator-test passed
``` 

## Reference

* [Haskell Offical Site][1]
* [Basics of Haskell][2]
* [Haskell Tool Stack][3]
* [Hspec][5]
* [Learn You a Haskell for Great Good!][6]
* [Haskell Wiki][8]

[1]: https://www.haskell.org
[2]: https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell
[3]: https://docs.haskellstack.org/
[4]: ./test/Calculator
[5]: https://hspec.github.io
[6]: https://learnyouahaskell.com/
[7]: https://twitter.com/bartoszmilewski
[8]: https://wiki.haskell.org/Haskell