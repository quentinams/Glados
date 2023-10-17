module ParseChar ( parseChar, parseAnyChar ) where

import ParserModule (Parser(..))

parseChar :: Char -> Parser Char
parseChar c = Parser $ \s -> case s of
    (x:xs) -> if x == c then Just (c, xs) else Nothing
    [] -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar chars = Parser $ \input -> case input of
    (x:xs) | x `elem` chars -> Just (x, xs)
    _ -> Nothing
