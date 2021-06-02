module Parser where

import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec

data JValue = JArray [JValue]
            | JNumber Integer
            | JString String
            | JObject [(String, JValue)]
            deriving Show

parseFile :: Parser JValue
parseFile = spaces *> content <?> "Json Object Failed"
    where
        content = parseArray -- <|> parseObject

parseSeq :: Char -> Parser a -> Char -> Parser [a]
parseSeq l p r = between (char l <* spaces) (char r) $
                 (p <* spaces) `sepBy` (char ',' <* spaces)

parseArray :: Parser JValue
parseArray = JArray <$> parseSeq '[' parseNumber ']'

parseNumber :: Parser JValue
parseNumber = (JNumber . read) <$> many1 digit

testParser :: String -> Either ParseError JValue
testParser = parse parseFile "wa"