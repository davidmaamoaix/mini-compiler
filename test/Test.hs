import Test.HUnit
import System.IO

import Lexer
import Parser

main = do
    print $ parseProgram "int main () { int abc = 1;}"