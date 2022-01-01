module ParserTest where

import Data.Either
import Test.HUnit

import Parser
import TestData

-- The parser test only tests for trivial pass/fail. The
-- correctness of the program is not tested, as that will
-- be covered when testing the emitted code (read: im lazy).

parserPass :: String -> Bool
parserPass s = isRight $ parseProgram s

parserTest :: Test
parserTest = TestList $ prep <$> fetchTests
    where
        fail = "Parser does not pass"
        prep pair = TestCase $ do
            (raw, pass) <- pair
            assertBool fail (pass == parserPass raw)
