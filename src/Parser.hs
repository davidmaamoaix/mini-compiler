{-# LANGUAGE GADTs #-}

module Parser where

import Lexer

import Control.Monad
import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.ParserCombinators.Parsec

type Parser a = Parsec [TokenPos] () a

data Assign
data Exp
data GlobDecl
data Stmt
data VarType
data LValue
data ValDecl = ValDecl { varType :: VarType
                       , varId :: String
                       }

data Syntax a where

    -- basic constructs
    DeclStruct :: String -> Syntax GlobDecl
    DefStruct :: String -> [ValDecl] -> Syntax GlobDecl
    GlobVar :: [ValDecl] -> Syntax GlobDecl
    TypeDef :: VarType -> String -> Syntax GlobDecl
    DeclVar :: ValDecl -> Syntax Exp -> Syntax ValDecl
    FuncDef :: ValDecl -> [ValDecl] -> Syntax GlobDecl
    IdType :: String -> Syntax VarType
    PtrType :: Syntax VarType -> Syntax VarType
    ArrType :: Syntax VarType -> Syntax VarType
    StcType :: String -> Syntax VarType

    -- left values
    IdVar :: String -> Syntax LValue
    StcMember :: Syntax LValue -> String -> Syntax LValue
    PtrMember :: Syntax LValue -> String -> Syntax LValue
    DerefVar :: Syntax LValue -> Syntax LValue
    AsnArr :: Syntax LValue -> Syntax Exp -> Syntax LValue

    -- expressions
    Lit :: ValueType -> Syntax ValueType
    FuncCall :: String -> [Syntax Exp] -> Syntax Exp

    -- statements
    StmtBlock :: [Stmt] -> Syntax [Stmt]
    IncStmt :: String -> Syntax Stmt
    DecStmt :: String -> Syntax Stmt
    ExpStmt :: Syntax Exp -> Syntax Stmt
    AsnStmt :: Syntax LValue -> String -> Exp -> Syntax Stmt
    DeclStmt :: VarDecl -> Syntax Stmt
    DefStmt :: VarDecl -> Syntax Exp -> Syntax Stmt
