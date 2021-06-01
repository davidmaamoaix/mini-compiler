module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec

file :: GenParser Char st [[String]]
file = line `endBy` char '\n'

line :: GenParser Char st [String]
line = cell `sepBy` char ','

cell :: GenParser Char st String
cell = many $ noneOf ",\n"

parseFile :: String -> Either ParseError [[String]]
parseFile = parse file "wat"