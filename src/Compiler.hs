module Compiler where

import Text.Parsec.Pos
import Text.Parsec.Error
import Control.Monad (guard)

import qualified Data.Map as M
import qualified Data.Set as S

import SSA
import Liveness
import RegAlloc
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
            let ir = toSSA ast
            let live = liveness (irCode ir)
            let interGraph = genInterGraph (irVars ir) live
            return $ show (gEdges interGraph)
            --let ordering = simpOrdering interGraph
            --return $ show $ simpOrdering (IGraph 6 (M.fromList [
            --    (1, S.fromList[2, 3]), (2, S.fromList[1, 3, 4]), (3, S.fromList[1, 2, 5]), (4, S.fromList[2, 3]), (5, S.fromList[3])]) M.empty)

convertParseError :: ParseError -> CompilerError
convertParseError pErr = SyntaxError (errPos pErr) (errInfo pErr)
    where
        errPos pErr = let p = errorPos pErr in (sourceLine p, sourceColumn p)
        errInfo pErr = formatParseError (errorMessages pErr)
