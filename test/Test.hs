import Test.HUnit

import Parser
import Compiler
import StaticCheck

import TestData
import ParserTest

main = do
    s <- readProgram "basics.l1"
    print $ toSSA <$> parseProgram s
    runTestTT parserTest
