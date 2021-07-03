module LexerTest where

import Test.HUnit

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Text.Parsec

import Lexer

import TestUtils

lexerTest :: CodeMap -> Test
lexerTest sample = TestList $ Maybe.catMaybes $ map ($ sample) [ lexerBasics
                                                               , lexerStructs
                                                               ]

simpleTester :: CodeMap -> String -> Either ParseError [Token] -> Maybe Test
simpleTester sample name tokens = testFor (\s -> (tokens, result s))
    where
        testFor = equalRun sample name ("Lexes " ++ name ++ ".c0")
        result x = stripPos <$> tokenize x name

lexerBasics :: CodeFileTest
lexerBasics sample = simpleTester sample "basics" tokens
    where
        tokens = Right [ Identifier "int"
                       , Identifier "add"
                       , Symbol '('
                       , Identifier "int"
                       , Identifier "a"
                       , Symbol ','
                       , Identifier "int"
                       , Identifier "b"
                       , Symbol ')'
                       , Symbol '{'
                       , Identifier "return"
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
                       , Identifier "return"
                       , Identifier "a"
                       , Operator "&"
                       , Operator "&"
                       , Identifier "b"
                       , Symbol ';'
                       , Symbol '}'
                       ]

lexerStructs :: CodeFileTest
lexerStructs sample = simpleTester sample "structs" tokens
    where
        tokens = Right [ Identifier "struct"
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
                       , Identifier "struct"
                       , Identifier "Foo"
                       , Identifier "a"
                       , Symbol ';'
                       , Identifier "a"
                       ]