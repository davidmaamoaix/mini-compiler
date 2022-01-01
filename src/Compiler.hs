module Compiler where

import Text.Parsec.Pos
import Text.Parsec.Error

import Parser (parseProgram)
import Text.ParserCombinators.Parsec (sourceColumn)
import StaticCheck (staticCheck)
import Control.Monad (guard)

formatParseError = showErrorMessages 
    "or" "unknown parse error"
    "expecting" "unexpected" "end of input" 

data CompilerError
    = SyntaxError { pos :: (Int, Int), info :: String }
    | StaticCheckError { line :: Int, info :: String }
    deriving Show

compile :: String -> Either CompilerError String
compile s = do
    ast <- either (Left . convertParseError) Right (parseProgram s)
    case staticCheck ast of
        Nothing -> return ""
        Just (line, msg) -> Left $ StaticCheckError line msg

convertParseError :: ParseError -> CompilerError
convertParseError pErr = SyntaxError (errPos pErr) (errInfo pErr)
    where
        errPos pErr = let p = errorPos pErr in (sourceLine p, sourceColumn p)
        errInfo pErr = formatParseError (errorMessages pErr)