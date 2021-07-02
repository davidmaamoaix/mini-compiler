import Test.HUnit

import Lexer
import Parser

import LexerTest
import TestUtils

main = do
    sample <- makeCodeMap []
    runTestTT $ lexerTest $ sample