module Compiler where

import Text.Parsec.Pos
import Text.Parsec.Error
import Control.Monad (guard)

import SSA
import RegAlloc (liveness)
import Parser (parseProgram)
import Text.ParserCombinators.Parsec (sourceColumn)
import StaticCheck (staticCheck)

formatParseError = showErrorMessages 
    "or" "unknown parse error"
    "expecting" "unexpected" "end of input" 

data CompilerError
    = SyntaxError { pos :: (Int, Int), info :: String }
    | StaticCheckError { info :: String }
    deriving Show

compile :: String -> Either CompilerError String
compile s = do
    ast <- either (Left . convertParseError) Right (parseProgram s)
    case staticCheck ast of
        Right _ -> convert ast
        Left msg -> Left $ StaticCheckError msg
    where
        convert ast = do
            let ssa = toSSA ast
            return $ show $ ssa

convertParseError :: ParseError -> CompilerError
convertParseError pErr = SyntaxError (errPos pErr) (errInfo pErr)
    where
        errPos pErr = let p = errorPos pErr in (sourceLine p, sourceColumn p)
        errInfo pErr = formatParseError (errorMessages pErr)
