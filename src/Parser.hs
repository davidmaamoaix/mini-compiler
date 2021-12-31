{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser where

import Lexer
import Debug.Trace

import Control.Monad
import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import qualified Text.Parsec.Token as Tok

data Prog
data Stmt
data Simp
data Exp
data LVal
data Decl

type Ident = String

-- maybe ADT this part
type AsnOp = String
type BinOp = String
type UnOp = String
type PostOp = String

data Node a where
    NProg :: [Node Stmt] -> Node Prog
    NBlockStmt :: [Node Stmt] -> Node Stmt
    NDeclStmt :: Node Decl -> Node Stmt
    NSimpStmt :: Node Simp -> Node Stmt
    NRetStmt :: Node Exp -> Node Stmt
    NDecl :: Ident -> Node Decl
    NDeclAsn :: Ident -> Node Exp -> Node Decl
    NSimp :: Node LVal -> AsnOp -> Node Exp -> Node Simp
    NIdL :: Ident -> Node LVal
    NIntExp :: Integer -> Node Exp
    NIdExp :: Ident -> Node Exp
    NBinExp :: Node Exp -> Ident -> Node Exp -> Node Exp
    NNegExp :: Node Exp -> Node Exp

deriving instance Eq (Node a)
deriving instance Show (Node a)

pProg :: Parser (Node Prog)
pProg = prefix *> braces (NProg <$> many pStmt)
    where
        prefix = reserved "int" *> reserved "main"
              *> reservedOp "(" *> reservedOp ")"

pStmt :: Parser (Node Stmt)
pStmt = try (NDeclStmt <$> pDecl)
    <|> try (NSimpStmt <$> pSimp)
    <|> reserved "return" *> (NRetStmt <$> pExp)

pDecl :: Parser (Node Decl)
pDecl = NDecl <$> ident

pSimp :: Parser (Node Simp)
pSimp = NSimp <$> pLVal <*> pAsnOp <*> pExp

pExp = undefined
pLVal = undefined
pAsnOp = undefined
