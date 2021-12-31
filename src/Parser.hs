{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser where

import Data.Functor

import Control.Monad
import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer

data Prog
data Stmt
data Simp
data Exp
data LVal
data Decl

type Ident = String

data BinOp = Add | Sub | Mul | Div | Mod deriving (Eq, Show)

data UnOp = Neg deriving (Eq, Show)

data AsnOp = Asn | AddAsn | SubAsn
           | MulAsn | DivAsn | ModAsn
           deriving (Eq, Show)

-- maybe ADT this part
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
    NBinExp :: Node Exp -> BinOp -> Node Exp -> Node Exp
    NNegExp :: Node Exp -> Node Exp

deriving instance Eq (Node a)
deriving instance Show (Node a)

binOp s out = Ex.Infix (reservedOp s $> out)
preOp s out = Ex.Prefix (reservedOp s $> out)
postOp s out = Ex.Postfix (reservedOp s $> out)

exprTable = 
    [
        [preOp "-" NNegExp],
        [
            binOp "*" (`NBinExp` Mul) Ex.AssocLeft,
            binOp "/" (`NBinExp` Div) Ex.AssocLeft,
            binOp "%" (`NBinExp` Mod) Ex.AssocLeft 
        ],
        [
            binOp "+" (`NBinExp` Add) Ex.AssocLeft,
            binOp "-" (`NBinExp` Sub) Ex.AssocLeft
        ]
    ]

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

expr :: Parser (Node Exp)
expr = Ex.buildExpressionParser exprTable pExp

pExp = try (NIntExp <$> decimal)
   <|> try (NIdExp <$> ident)
   <|> parens expr

pLVal = undefined
pAsnOp = undefined
