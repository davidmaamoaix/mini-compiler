module TestData where

import System.IO
import qualified Data.Map as Map

import Text.Parsec
import Test.HUnit

import Lexer

type CodeMap = [(String, String)]
type StateRef = Map.Map String

codeDir = "c0_sample/"

codeFiles = [ "basics.c0"
            , "structs.c0"
            , "number_error.c0"
            , "pointer.c0"
            ]

makeCodeMap :: [String] -> IO CodeMap
makeCodeMap s = do
    code <- sequence [readFile $ codeDir ++ i | i <- s]
    return $ zip s code

tokenRef :: StateRef (Either ParseError [TokenPos])
tokenRef = Right <$> Map.fromList entries
    where
        entries = [ ("basics.c0", basics)
                  , ("structs.c0", structs)
                  , ("number_error.c0", numberError)
                  , ("pointer.c0", pointer)
                  ]
        basics = []
        structs = []
        numberError = []
        pointer = []
