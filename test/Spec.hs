import Lexer
import Parser

main :: IO ()
main = do
    putStrLn $ show $ testLexer identifier "asdhas asdhas"
    putStrLn $ show $ testParser "  { \" abc \" : \"hello\" , \"def\" : [{\"123\": []}] }  "