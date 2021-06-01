module Parser where

import Text.ParserCombinators.Parsec

file :: GenParser Char st [[String]]
file = line `endBy` eol

line :: GenParser Char st [String]
line = cell `sepBy` char ','

cell :: GenParser Char st String
cell = many $ noneOf ",\n"

eol :: GenParser Char st String
eol = try (string "\n\r") <|> string "\n"

parseFile :: String -> Either ParseError [[String]]
parseFile = parse file "wat"