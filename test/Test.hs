import Test.HUnit

import Parser
import Compiler
import StaticCheck
import SSA

import TestData
import ParserTest

main = do
    -- s <- readProgram "fibonacci.l1"
    -- print $ toSSA <$> parseProgram s
    runTestTT parserTest
