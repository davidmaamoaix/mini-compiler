module Compiler where

import Data.List (intercalate)
import Text.Parsec.Pos
import Text.Parsec.Error
import Control.Monad (guard)

import qualified Data.Map as M
import qualified Data.Set as S

import SSA
import Liveness
import RegAlloc
import CodeGen (codeGen)
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
            let ir = toSSA ast :: IR AsmReg
            let coloring = colorRegisters ir
            let code = codeGen (irCode ir) coloring
            return $ intercalate "\n" code

convertParseError :: ParseError -> CompilerError
convertParseError pErr = SyntaxError (errPos pErr) (errInfo pErr)
    where
        errPos pErr = let p = errorPos pErr in (sourceLine p, sourceColumn p)
        errInfo pErr = formatParseError (errorMessages pErr)
