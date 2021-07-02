import Lexer
import Parser

main :: IO ()
main = do
    putStrLn $ show $ testLexer tokenParser "true"
    putStrLn $ show $ testParser "  { \" abc \" : \"hello\" , \"def\" : [{\"123\": []}] }  "