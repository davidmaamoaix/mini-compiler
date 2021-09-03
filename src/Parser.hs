{-# LANGUAGE GADTs #-}

module Parser where

import Lexer

import Control.Monad
import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.ParserCombinators.Parsec

type Parser a = Parsec [TokenPos] () a

data Syntax a where
    Lit :: ValueType -> Syntax ValueType
