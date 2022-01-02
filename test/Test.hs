import Test.HUnit

import Parser
import Compiler
import StaticCheck
import SSA

import TestData
import ParserTest

main = do
    s <- readProgram "flow_intercept.l1"
    print $ compile s
    runTestTT parserTest
