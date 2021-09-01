module LexerTest where

import Test.HUnit

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Text.Parsec

import Lexer
import TestData

testLexer :: IO Test
testLexer = do
    code <- codeList
    return $ TestList $ (TestCase . makeAssert) <$> code
    where
        makeAssert (a, b) = assertEqual "Token list mismatch" a b
        codeMap = makeCodeMap codeFiles
        codeList :: IO [(Either ParseError [TokenPos], Either ParseError [TokenPos])]
        codeList = do
            code <- codeMap
            return [(tokenize c, Map.findWithDefault (Right []) f tokenRef) | (f, c) <- code]
