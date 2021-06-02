import Parser

main :: IO ()
main = do
    putStrLn $ show $ testParser " [  1  , 2  , 3  ]   "