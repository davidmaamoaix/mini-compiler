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
data VarDecl = VarDecl { varType :: String
                       , varId :: String
                       }

data Syntax a where
    Lit :: ValueType -> Syntax ValueType
    FuncCall :: String -> [Syntax Exp] -> Syntax Exp
    DeclStruct :: String -> Syntax GlobDecl
    DefStruct :: String -> [VarDecl] -> Syntax GlobDecl
    DeclVar :: [VarDecl] -> Syntax VarDecl
    GlobVar :: Syntax VarDecl -> Syntax GlobDecl
    TypeDef :: String -> String -> Syntax GlobDecl