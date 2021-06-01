module Parser where

import Text.ParserCombinators.Parsec

file :: GenParser Char st [[String]]
file = do
    result <- many line
    eof
    return result

line :: GenParser Char st [String]
line = do
    result <- cells
    char '\n'
    return result

cells :: GenParser Char st [String]
cells = do
    first <- cell
    remain <- cellRemain
    return $ first : remain

cell :: GenParser Char st String
cell = many $ noneOf ",\n"

cellRemain :: GenParser Char st [String]
cellRemain = (char ',' >> cells) <|> return []

parseFile :: String -> Either ParseError [[String]]
parseFile = parse file "wat"