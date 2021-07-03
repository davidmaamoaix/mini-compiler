module Lexer where

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.Token
import Text.ParserCombinators.Parsec hiding (token, tokens, try)

import Control.Applicative hiding (many, (<|>))

data VarType = Int
             | Bool
             | String
             | Char
             deriving (Eq, Show)

data Token = Identifier String
           | NumberLit Int
           | StringLit String
           | CharLit Char
           | BoolLit Bool
           | Operator String
           | Keyword String
           | Symbol Char
           deriving (Eq, Show)

type TokenPos = (Token, SourcePos)

parserPos :: Parser Token -> Parser TokenPos
parserPos p = flip (,) <$> getPosition <*> p

idParser :: Parser TokenPos
idParser = do
    let first = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
        rest = first ++ ['0'..'9']

    pos <- getPosition
    firstChar <- oneOf first
    restChar <- many $ oneOf rest
    spaces
    return (Identifier $ firstChar : restChar, pos)

intParser :: Parser TokenPos
intParser = parserPos $ NumberLit <$> readNum
    where
        readNum = read <$> (return <$> char '0' <|> many1 digit)

escape :: Parser Char
escape = char '\\' *> (replace <$> oneOf "\\\"'?ntvbrfa")
    where
        -- fromJust because it can't possibly mess up
        replace = Maybe.fromJust . find
        find x = Map.lookup x $ Map.fromList [ ('\\', '\\')
                                             , ('"', '"')
                                             , ('\'', '\'')
                                             , ('?', '?')
                                             , ('n', '\n')
                                             , ('t', '\t')
                                             , ('v', '\v')
                                             , ('b', '\b')
                                             , ('r', '\r')
                                             , ('f', '\f')
                                             , ('a', '\a')
                                             ]

nonEscape :: Parser Char
nonEscape = noneOf "\\\""

sCharParser :: Parser Char
sCharParser = nonEscape <|> escape

stringParser :: Parser TokenPos
stringParser = parserPos $ between (char '"') (char '"') content
    where
        content = StringLit <$> many sCharParser

charParser :: Parser TokenPos
charParser = parserPos $ CharLit <$> between (char '\'') (char '\'') content
    where
        content = sCharParser <|> char '"' <|> char '\0'

boolParser :: Parser TokenPos
boolParser = parserPos $ BoolLit <$> (true *> return True <|> false *> return False)
    where
        true = try $ string "true"
        false = try $ string "false"

opParser :: Parser TokenPos
opParser = parserPos $ Operator <$> (choice $ string <$> ops)
    where
        ops = [ "!", "~", "-", "*", "+"
              , "/", "%", "<<", ">>", "<"
              , ">", "==", "!=", "&", "^"
              , "|", "&&", "||", "=", "+="
              , "-=", "*=", "/=", "%=", "<<="
              , ">>=", "&=", "^=", "|="
              ]

lParamParser, rParamParser, lBraceParser, rBraceParser, commaParser,  semiParser :: Parser TokenPos
lParamParser = parserPos $ Symbol <$> char '('
rParamParser = parserPos $ Symbol <$> char ')'
lBraceParser = parserPos $ Symbol <$> char '{'
rBraceParser = parserPos $ Symbol <$> char '}'
commaParser = parserPos $ Symbol <$> char ','
semiParser = parserPos $ Symbol <$> char ';'

tokenParser :: Parser TokenPos
tokenParser = choice [ lParamParser
                     , rParamParser
                     , lBraceParser
                     , rBraceParser
                     , commaParser
                     , semiParser
                     , intParser
                     , stringParser
                     , charParser
                     , boolParser
                     , opParser
                     , idParser
                     ]

tokensParser :: Parser [TokenPos]
tokensParser = spaces *> many (tokenParser <* spaces)

stripPos :: [TokenPos] -> [Token]
stripPos = map fst

tokenize :: String -> String -> Either ParseError [TokenPos]
tokenize code source = parse tokensParser source code