import Test.HUnit

import Compiler
import TestData
import ParserTest

main = do
    s <- readProgram "number_error.l1"
    print $ compile s
    runTestTT parserTest
