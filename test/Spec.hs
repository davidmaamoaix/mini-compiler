import Parser

main :: IO ()
main = do
    putStrLn $ show $ parseFile "a, b, c\n"
