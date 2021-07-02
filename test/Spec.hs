import Lexer
import Parser

main :: IO ()
main = do
    putStrLn $ show $ testLexer tokenParser "\"\n\""
    putStrLn $ show $ testParser "  { \" abc \" : \"hello\" , \"def\" : [{\"123\": []}] }  "