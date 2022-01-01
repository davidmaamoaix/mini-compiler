module Lexer where

import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import qualified Text.Parsec.Token as Tok

resNames :: [String]
resNames =
    [ "struct", "typedef", "if", "else"
    , "while", "for", "continue", "break"
    , "return", "assert", "true", "false"
    , "NULL", "alloc", "alloc_array", "int"
    , "bool", "void", "char", "string"
    ]

resOps :: [String]
resOps =
    [ "!", "~", "-", "+", "*", "/", "%", "<<", ">>"
    , "<", ">", ">=", "<=", "==", "!=", "&", "^"
    , "|", "&&", "||", "=", "+=", "-=", "*=", "/="
    , "%=", "<<=", ">>=", "&=", "|=", "^=", "->"
    , ".", "--", "++", "[", "]", ";", "?", ":"
    ]

lexDef :: Tok.LanguageDef ()
lexDef = Tok.LanguageDef
    { Tok.commentStart = "/*"
    , Tok.commentEnd = "*/"
    , Tok.commentLine = "//"
    , Tok.nestedComments = False
    , Tok.identStart = letter
    , Tok.identLetter = alphaNum <|> char '_'
    , Tok.opStart = oneOf "!~-+*/%<>=&|[]{}();?:^"
    , Tok.opLetter = oneOf "-+<>=&|"
    , Tok.reservedNames = resNames
    , Tok.reservedOpNames = resOps
    , Tok.caseSensitive = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser lexDef

ident = Tok.identifier lexer
parens = Tok.parens lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
semi = Tok.lexeme lexer $ string ";"
braces = Tok.braces lexer
decimal = Tok.lexeme lexer $ Tok.decimal lexer
whiteSpace = Tok.whiteSpace lexer
