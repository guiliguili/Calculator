module Calculator.LexerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Calculator.Lexer

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "tokenize" $
    it "tokenizing number" $
      tokenize "1" `shouldBe` [TokNum 1.0]