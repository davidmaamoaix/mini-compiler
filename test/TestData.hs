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

tokenRef :: StateRef (Either ParseError [Token])
tokenRef = Right <$> Map.fromList entries
    where
        entries = [ ("basics.c0", basics)
                  , ("structs.c0", structs)
                  , ("number_error.c0", numberError)
                  , ("pointer.c0", pointer)
                  ]

        basics =        [ Identifier "int"
                        , Identifier "add"
                        , Symbol '('
                        , Identifier "int"
                        , Identifier "a"
                        , Symbol ','
                        , Identifier "int"
                        , Identifier "b"
                        , Symbol ')'
                        , Symbol '{'
                        , Keyword "return"
                        , Identifier "a"
                        , Operator "+"
                        , Identifier "b"
                        , Symbol ';'
                        , Symbol '}'
                        , Identifier "bool"
                        , Identifier "and_gate"
                        , Symbol '('
                        , Identifier "bool"
                        , Identifier "a"
                        , Symbol ','
                        , Identifier "bool"
                        , Identifier "b"
                        , Symbol ')'
                        , Symbol '{'
                        , Keyword "return"
                        , Identifier "a"
                        , Operator "&"
                        , Operator "&"
                        , Identifier "b"
                        , Symbol ';'
                        , Symbol '}'
                        ]

        structs =       [ Keyword "struct"
                        , Identifier "Foo"
                        , Symbol '{'
                        , Identifier "int"
                        , Operator "*"
                        , Identifier "ptr"
                        , Symbol ';'
                        , Identifier "string"
                        , Identifier "name"
                        , Symbol ';'
                        , Symbol '}'
                        , Symbol ';'
                        , Identifier "int"
                        , Identifier "main"
                        , Symbol '('
                        , Symbol ')'
                        , Symbol '{'
                        , Keyword "struct"
                        , Identifier "Foo"
                        , Identifier "a"
                        , Symbol ';'
                        , Identifier "a"
                        ]

        numberError =   [ Identifier "int"
                        , Identifier "main"
                        , Symbol '('
                        , Symbol ')'
                        , Symbol '{'
                        , Identifier "int"
                        , Identifier "a"
                        , Operator "="
                        , Literal (IntLit 0)
                        , Literal (IntLit 0)
                        , Symbol ';'
                        , Symbol '}'
                        ]

        pointer =       [ Identifier "int"
                        , Identifier "main"
                        , Symbol '('
                        , Symbol ')'
                        , Symbol '{'
                        , Identifier "int"
                        , Operator "*"
                        , Identifier "b"
                        , Operator "="
                        , Keyword "alloc"
                        , Symbol '('
                        , Identifier "int"
                        , Symbol ')'
                        , Symbol ';'
                        , Operator "*"
                        , Identifier "b"
                        , Operator "="
                        , Literal (IntLit 20)
                        , Symbol ';'
                        , Identifier "int"
                        , Identifier "a"
                        , Operator "="
                        , Operator "*"
                        , Identifier "b"
                        , Symbol ';'
                        , Symbol '}'
                        ]
