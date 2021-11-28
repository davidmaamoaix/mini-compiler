{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser where

import Lexer

import Control.Monad
import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import qualified Text.Parsec.Token as Tok

data GDecl
data Prog
data Param
data TypeDef
data SDecl
data SDef
data Field
data Type
data Block -- not coerced into [Stmt] due to scope check convenience
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
    NSIdType :: Ident -> Node Type
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
    NIf :: Node Exp -> Node Stmt -> Maybe (Node Stmt) -> Node Stmt
    NWhile :: Node Exp -> Node Stmt -> Node Stmt
    NFor :: Maybe (Node Simp) -> Node Exp -> Maybe (Node Simp) -> Node Stmt -> Node Stmt
    NRet :: Node Exp -> Node Stmt
    NRetNil :: Node Stmt
    NAssert :: Node Exp -> Node Stmt
    NBreak :: Node Stmt
    NCont :: Node Stmt
    NIntExp :: Integer -> Node Exp
    NBoolExp :: Bool -> Node Exp
    NIdentExp :: Ident -> Node Exp
    NNilExp :: Node Exp
    NUnExp :: UnOp -> Node Exp -> Node Exp
    NBinExp :: Node Exp -> UnOp -> Node Exp -> Node Exp
    NIfExp :: Node Exp -> Node Exp -> Node Exp -> Node Exp
    NCallExp :: Ident -> [Node Exp] -> Node Exp
    NDotExp :: Node Exp -> Ident -> Node Exp
    NArrExp :: Node Exp -> Ident -> Node Exp
    NAllocExp :: Node Type -> Node Exp
    NDerefExp :: Node Exp -> Node Exp
    NAllocArrayExp :: Node Type -> Node Exp -> Node Exp
    NIndexExp :: Node Exp -> Node Exp -> Node Exp

deriving instance Eq (Node a)
deriving instance Show (Node a)

programP :: Parser (Node Prog)
programP = NProg <$> many gDeclP

gDeclP :: Parser (Node GDecl)
gDeclP = choice declParses
    where
        fieldList = between (char '{') (char '}') (many fieldP)
        declParses = [ try $ reserved "struct" *> (NSDecl <$> ident)
                     , try $ NSDef <$> (reserved "struct" *> ident) <*> fieldList
                     ]

fieldP :: Parser (Node Field)
fieldP = NField <$> typeP <*> ident

typeP :: Parser (Node Type)
typeP = choice typeParses
    where
        typeParses = [ reserved "struct" *> (NSIdType <$> ident)
                     , NIdType <$> ident
                     , reserved "int" *> pure NIntType
                     , reserved "bool" *> pure NBoolType
                     , reserved "void" *> pure NVoidType
                     , (NPtrType <$> typeP) <* reservedOp "*"
                     , (NArrType <$> typeP) <* reservedOp "[" <* reservedOp "]"
                     ]

litExpP :: Parser (Node Exp)
litExpP = choice litParses
    where
        litParses = [ NIntExp <$> Tok.natural lexer 
                    , reserved "NULL" *> pure NNilExp
                    , reserved "true" *> pure (NBoolExp True)
                    , reserved "false" *> pure (NBoolExp False)
                    ]

parseProg :: String -> Either ParseError (Node Prog)
parseProg = parse programP ""

