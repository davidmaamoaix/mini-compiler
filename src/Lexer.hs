module Lexer where

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.Token
import Text.ParserCombinators.Parsec hiding (token, tokens, try)

import Control.Applicative hiding (many, (<|>))

data LitType = StrLit String
             | IntLit Int
             | BoolLit Bool
             | CharLit Char
             | Null
             deriving (Eq, Show)

data Token = Identifier String
           | Literal LitType
           | Operator String
           | Keyword String
           | Symbol Char
           deriving (Eq, Show)

type TokenPos = (Token, SourcePos)

keywords = [ "struct", "typedef", "if", "else", "while",
             "for", "continue", "break", "return", "assert",
             "alloc", "alloc_array"
           ]

reser = keywords ++ [ "true", "false", "NULL", "int",
                      "bool", "string", "char"
                    ]

token :: TokenPos -> Token
token = fst

pos :: TokenPos -> SourcePos
pos = snd

parserPos :: Parser Token -> Parser TokenPos
parserPos p = flip (,) <$> getPosition <*> p

idParser :: Parser Token
idParser = do
    s <- ((:) <$> oneOf first <*> (many $ oneOf rest))
    return $ (if s `elem` reser then Keyword else Identifier) s

    where
        first = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
        rest = first ++ ['0'..'9']

decParser :: Parser Token
decParser = Literal . IntLit . read <$> num
    where
        first = oneOf ['1'..'9']
        rest = oneOf ['0'..'9']
        num = (string "0" <|> (:) <$> first <*> (many $ rest))

hexParser :: Parser String
hexParser = char '0' *> oneOf "xX" *> (many $ oneOf hex)
    where
        hex = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

escParser :: Parser Char
escParser = choice $ (convert) <$> [ "\\n", "\\t", "\\v", "\\b",
                                     "\\r", "\\f", "\\a", "\\",
                                     "\\?", "\\'", "\\\""
                                   ]
    where
        convert :: String -> Parser Char
        convert s = readEsc <$> (try $ string s)
        readEsc s = read ("'" ++ s ++ "'")

strParser :: Parser String
strParser = string "a"

keyParser :: Parser String
keyParser = choice $ (try . string) <$> keywords

stripPos :: [TokenPos] -> [Token]
stripPos = map fst
