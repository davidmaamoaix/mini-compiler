module Lexer where

import Data.Functor

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

pDecimal :: Parser Integer 
pDecimal = try dec 
       <|> char '0' $> 0
    where
        dec = do
            pre <- oneOf "123456789"
            suf <- many $ oneOf "0123456789"
            return $ read (pre : suf)

lexeme = Tok.lexeme lexer
ident = Tok.identifier lexer
parens = Tok.parens lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
semi = Tok.lexeme lexer $ char ';'
braces = Tok.braces lexer
decimal = lexeme pDecimal
whiteSpace = Tok.whiteSpace lexer
