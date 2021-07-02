module LexerTest where

import Test.HUnit

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Lexer

import TestUtils

lexerTest :: CodeMap -> Test
lexerTest sample = TestList $ Maybe.catMaybes $ map ($ sample) [lexerBasics]

lexerBasics :: CodeFileTest
lexerBasics sample = equalRun sample "basics" "Lexes basics.c0" (\s -> (tokens, tokenize s "basics"))
    where
        tokens = Right []