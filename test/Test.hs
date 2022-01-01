import Test.HUnit
import System.IO

import Lexer
import Parser

import TestData
import ParserTest

main = do
    runTestTT parserTest
    