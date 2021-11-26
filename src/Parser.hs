{-# LANGUAGE GADTs #-}

module Parser where

import Lexer

import Control.Monad
import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.ParserCombinators.Parsec

data GDecl
data Prog
data Param
data TypeDef
data SDecl
data SDef
data Field
data Type
data Block
data Stmt
data Simp
data LVal
data Else
data Exp

type Ident = String

-- maybe ADT this part
type AsnOp = String
type BinOp = String
type UnOp = String
type PostOp = String

data Node a where
    NProg :: [Node GDecl] -> Node Prog
    NFDecl :: Node Type -> Ident -> [Node Param] -> Node GDecl
    NFDef :: Node Type -> Ident -> [Node Param] -> Node Block -> Node GDecl
    NParam :: Node Type -> Ident -> Node Param
    NTypeDef :: Node Type -> Ident -> Node TypeDef
    NSDecl :: Ident -> Node GDecl
    NSDef :: Ident -> [Node Field] -> Node GDecl
    NField :: Node Type -> Ident -> Node Field
    NIntType :: Node Type
    NBoolType :: Node Type
    NVoidType :: Node Type
    NIdType :: Ident -> Node Type
    NPtrType :: Node Type -> Node Type
    NArrType :: Node Type -> Node Type
    NBlock :: [Node Stmt] -> Node Block
    NSimpStmt :: Node Simp -> Node Stmt
    NBlockStmt :: Node Block -> Node Stmt
    NAsnSimp :: Node LVal -> AsnOp -> Node Exp -> Node Simp
    NPostSimp :: Node LVal -> PostOp -> Node Simp
    NDecl :: Node Type -> Ident -> Node Simp
    NAsnDecl :: Node Type -> Ident -> Node Exp -> Node Simp
    NExpSimp :: Node Exp -> Node Simp
    NIdLVal :: Ident -> Node LVal
    NDotLVal :: Node LVal -> Ident -> Node LVal
    NArrLVal :: Node LVal -> Ident -> Node LVal
    NDeref :: Node LVal -> Node LVal
    NIndexLVal :: Node LVal -> Node Exp -> Node LVal
