module ParseOr (parseOr, parseAnyCharUsingOr) where
import ParseChar

import Types


parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 input =
    case p1 input of
        Nothing -> p2 input
        success -> success


parseAnyCharUsingOr :: String -> Parser Char
parseAnyCharUsingOr chars input = 
    case filterChars chars input of
        Just (c, rest) -> Just (c, rest)
        Nothing -> Nothing

-- Une fonction auxiliaire qui tente de trouver le premier caractère de la liste 'chars' dans la chaîne 'input'
filterChars :: String -> String -> Maybe (Char, String)
filterChars _ [] = Nothing
filterChars chars (x:xs)
    | x `elem` chars = Just (x, xs)
    | otherwise = filterChars chars xs
