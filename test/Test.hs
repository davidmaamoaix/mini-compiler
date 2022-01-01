import Test.HUnit
import System.IO

import Lexer
import Parser

import TestData
import ParserTest

main = do
    s <- readProgram "div_by_zero.l1"
    runTestTT parserTest