module TestData where

import System.IO

import Parser

testCodeDir = "c0_sample/"
tests = 
    [ ("basics.l1", True)
    , ("number_error.l1", False)
    , ("div_by_zero.l1", True)
    , ("fibonacci.l1", True)
    , ("operators.l1", True)
    ]

fetchTests :: [IO (String, Bool)]
fetchTests = fmap conv tests
    where
        conv (path, pass) = do
            s <- readProgram path
            return (s, pass)

readProgram :: String -> IO String
readProgram path = do
    readFile $ testCodeDir ++ path
