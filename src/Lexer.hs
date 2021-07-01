module Lexer where

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
           deriving (Eq, Show)

type TokenPos = (Token, SourcePos)

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