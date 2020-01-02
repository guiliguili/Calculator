# haskell-calculator

## Overview
This demonstrates how to implement a simple Calculator with [Haskell][1].

It follows the [Basics of Haskell][2] tutorial.

## Building with Haskell Tool Stack

haskell-calculator uses the [Haskell Tool Stack][3] to setup, build, test and run the project.

## Running

### From Command Line

When you run the calculator from the command line, it will continuously prompt for an arithmetic expression, parse and evaluate it.
  
```bash
$ stack exec haskell-calculator-exe
1+2
3.0
```

### From REPL

You can also interactively test lower level function from GHCi as follow :

```bash
$ stack ghci
[...]
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> tokenize "1+2"
[TokNum 1.0,TokOp Plus,TokNum 2.0]
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> parse $ tokenize "1+2"
SumNode Plus (NumNode 1.0) (NumNode 2.0)
*Main Calculator.Evaluator Calculator.Lexer Calculator.Parser> evaluate (parse $ tokenize "a = 1+2") M.empty
Evaluation: (3.0,fromList [("a",3.0)])
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

[1]: https://www.haskell.org
[2]: https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell
[3]: https://docs.haskellstack.org/
[4]: ./test/Calculator
[5]: https://hspec.github.io