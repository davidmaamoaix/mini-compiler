import Test.HUnit
import System.IO

import Lexer
import Parser

main = do
    putStrLn $ show $ parseProg "struct Acc {foo**[]**[][  ]   a; bar b;};"