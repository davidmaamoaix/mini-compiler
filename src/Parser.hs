{-# LANGUAGE GADTs #-}

module Parser where

import Lexer

import Control.Monad
import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.ParserCombinators.Parsec

data GDecl
data Prog
data FDecl
data FDef
data Param
data Typedef
data SDecl
data SDef
data Field
data Type
data Block
data Decl
data Stmt
data Simp
data LVal
data Else
data Cont
data Exp

data Node a where
    NProg :: [Node GDecl] -> Node Prog
