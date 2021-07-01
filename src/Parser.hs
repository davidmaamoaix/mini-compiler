module Parser where

import Lexer

import Control.Monad
import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.ParserCombinators.Parsec

type ParserP a = Parsec [TokenPos] () a

nextPos :: SourcePos -> a -> [TokenPos] -> SourcePos
nextPos x _ [] = x
nextPos _ _ ((_, x) : _) = x

satisfyP :: (Token -> Bool) -> ParserP Token
satisfyP f = tokenPrim show nextPos result
    where
        result = \x -> if f $ fst x then Just $ fst x else Nothing

data JValue = JArray [JValue]
            | JNumber Integer
            | JString String
            | JObject [(String, JValue)]
            deriving Show

parseFile :: Parser JValue
parseFile = spaces *> content <?> "Json File Failed"
    where
        content = parseArray <|> parseObject

parseSeq :: Char -> Parser a -> Char -> Parser [a]
parseSeq l p r = between (char l <* spaces) (char r) $
                 (p <* spaces) `sepBy` (char ',' <* spaces)

parseAny :: Parser JValue
parseAny = parseArray
       <|> parseNumber
       <|> parseString
       <|> parseObject

parseArray :: Parser JValue
parseArray = JArray <$> parseSeq '[' parseAny ']'

parseNumber :: Parser JValue
parseNumber = (JNumber . read) <$> many1 digit

parseStringRaw :: Parser String
parseStringRaw = between (char '"') (char '"') (many $ noneOf "\"")

parseString :: Parser JValue
parseString = JString <$> parseStringRaw

parseObject :: Parser JValue
parseObject = JObject <$> parseSeq '{' parseEntry '}'
    where
        parseEntry = (,) <$> (parseStringRaw <* spaces <* char ':' <* spaces) <*> parseAny

testParser :: String -> Either ParseError JValue
testParser = parse parseFile "(stdio)"