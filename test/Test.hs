import Test.HUnit

import Parser
import Compiler
import StaticCheck
import SSA
import RegAlloc

import TestData
import ParserTest

import qualified Data.Set as S
import qualified Data.Map as M

g :: InterGraph Int
g = greedyColor $ IGraph 6 (M.fromList [(1, S.fromList[2, 3]), (2, S.fromList[1, 3, 4]), (3, S.fromList[1, 2, 5]), (4, S.fromList[2, 3]), (5, S.fromList[3])]) M.empty

main = do
    s <- readProgram "basics.l1"
    print $ compile s
    print $ gColor g
    runTestTT parserTest
