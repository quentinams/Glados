module ParseChar ( parseChar, parseAnyChar ) where


import Types

parseChar :: Char -> Parser Char
parseChar c (x:xs) | c == x = Just (c, xs)
parseChar _ _ = Nothing


parseAnyChar :: String -> Parser Char
parseAnyChar chars (x:xs)
    | x `elem` chars = Just (x, xs)
    | otherwise = parseAnyChar chars xs
parseAnyChar _ [] = Nothing


