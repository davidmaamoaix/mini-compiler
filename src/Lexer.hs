module Lexer where

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Data.Char (toUpper)

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
decParser = (Literal . IntLit . read) <$> num
    where
        first = oneOf ['1'..'9']
        rest = oneOf ['0'..'9']
        num = (string "0" <|> (:) <$> first <*> (many $ rest))

hexParser :: Parser Token
hexParser = (Literal . IntLit) <$> value
    where
        value = (char '0' *> oneOf "xX" *> (getHex <$> (many $ oneOf hex)))
        hex = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
        getHex s = run $ reverse s
        run [] = 0
        run (x:xs) = hexChar (toUpper x) + 16 * run xs
        hexChar '0' = 0
        hexChar '1' = 1
        hexChar '2' = 2
        hexChar '3' = 3
        hexChar '4' = 4
        hexChar '5' = 5
        hexChar '6' = 6
        hexChar '7' = 7
        hexChar '8' = 8
        hexChar '9' = 9
        hexChar 'A' = 10
        hexChar 'B' = 11
        hexChar 'C' = 12
        hexChar 'D' = 13
        hexChar 'E' = 14
        hexChar 'F' = 15
        hexChar _ = error "invalid char"

escParser :: Parser Char
escParser = choice $ (convert) <$> [ "\\n", "\\t", "\\v", "\\b",
                                     "\\r", "\\f", "\\a", "\\"
                                   ]
    where
        convert :: String -> Parser Char
        convert s = readEsc <$> (try $ string s)
        readEsc s = read ("'" ++ s ++ "'")

nCharParser :: Parser Char
nCharParser = noneOf "\\\"\n"

strParser :: Parser Token
strParser = (Literal . StrLit) <$> (char '"' *> many schar <* char '"')
    where
        schar = nCharParser <|> escParser

charParser :: Parser Token
charParser = (Literal . CharLit) <$> (char '\'' *> (cchar) <* char '\'')
    where
        cchar = nCharParser <|> escParser <|> char '"' <|> char '\0'

keyParser :: Parser Token
keyParser = Keyword <$> (choice $ (try . string) <$> keywords)

stripPos :: [TokenPos] -> [Token]
stripPos = map fst

tokenParser :: Parser TokenPos
tokenParser = choice $ (try . parserPos) <$> [ keyParser,
                                               charParser,
                                               strParser
                                             ]

tokenize :: String -> Either ParseError [TokenPos]
tokenize = parse (many tokenParser) ""