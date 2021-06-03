module Lexer where

import Text.Parsec.String
import Text.Parsec.Language

import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef {
    Token.commentLine = "//",
    Token.commentStart = "/*",
    Token.commentEnd = "*/",
    Token.caseSensitive = True,
    Token.reservedNames = [
        "true",
        "false",
        "NULL",
        "alloc",
        "alloc_array",
        "return",
        "break",
        "continue",
        "if",
        "while",
        "for",
        "struct",
        "void",
        "bool",
        "int",
        "typedef"
    ],
    Token.reservedOpNames = [
        "+", "-", "*", "/", "%", "&", "|", "^",
        "&&", "||", "<<", ">>", "<", ">", "<=",
        ">=", "==", "!=", "!", "~", "-=", "+=",
        "*=", "/=", "%=", "&=", "|=", "^=", "<<=",
        ">>=", "=", "++", "--", ";", ".", "->"
    ]
}


integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

operator :: String -> Parser ()
operator = Token.reservedOp lexer
