module TestUtils where

import System.IO

import Test.HUnit

import qualified Data.Map as Map

type CodeMap = Map.Map String String
type CodeFileTest = CodeMap -> Maybe Test

codeDir = "c0_sample/"

equalRun :: (Eq a, Show a) => CodeMap -> String -> String -> (String -> (a, a)) -> Maybe Test
equalRun sample file msg test = testCase <$> Map.lookup file sample
    where
        testCase code = TestCase $ assertEqual msg expected actual
            where
                (expected, actual) = test code

makeCodeMap :: [String] -> IO CodeMap
makeCodeMap s = (Map.fromList . zip s) <$> (sequence $ map getFile s)
    where
        getFile x = readFile (codeDir ++ x ++ ".c0")