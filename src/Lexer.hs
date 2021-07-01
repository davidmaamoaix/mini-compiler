module Lexer where

import Text.Parsec
import Text.Parsec.Token
import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec hiding (token, tokens)

data VarType = Int
             | Bool
             | String
             | Char
             deriving (Eq, Show)

data Token = Identifier String
           | Number Int
           | StringLit String
           | CharLit Char
           | BoolLit Bool
           | Type VarType
           | UnOp String
           | BinOp String
           | AsnOp String
           | Struct
           | TypeDef
           | If
           | Else
           | While
           | For
           | Continue
           | Break
           | Return
           | Assert
           | NULL
           | Alloc
           | AllocArray
           | LBrace
           | RBrace
           | Semicolon
           deriving (Eq, Show)

type TokenPos = (Token, SourcePos)
type ParserP a = Parsec [TokenPos] () a

parserPos :: Parser Token -> Parser TokenPos
parserPos p = (,) <$> p <*> getPosition

nextPos :: SourcePos -> a -> [TokenPos] -> SourcePos
nextPos x _ [] = x
nextPos _ _ ((_, x) : _) = x

satisfyP :: (Token -> Bool) -> ParserP Token
satisfyP f = tokenPrim show nextPos result
    where
        result = \x -> if f $ fst x then Just $ fst x else Nothing

identifier :: Parser TokenPos
identifier = do
    let first = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
        rest = first ++ ['0'..'9']

    pos <- getPosition
    firstChar <- oneOf first
    restChar <- many $ oneOf rest
    spaces
    return (Identifier $ firstChar : restChar, pos)

testLexer :: Parser TokenPos -> String -> Either ParseError TokenPos
testLexer lexer = parse lexer "(stdio)"