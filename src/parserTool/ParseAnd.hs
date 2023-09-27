module ParseAnd (parseAnd, parseAndWith, parseMany, parseSome) where
import Types

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 input =
    case p1 input of
        Nothing -> Nothing
        Just (result1, remaining1) ->
            case p2 remaining1 of
                Nothing -> Nothing
                Just (result2, remaining2) -> Just ((result1, result2), remaining2)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 input =
    case parseAnd p1 p2 input of
        Nothing -> Nothing
        Just ((result1, result2), remaining) -> Just (f result1 result2, remaining)

parseMany :: Parser a -> Parser [a]
parseMany p input =
    case p input of
        Just (result, remaining) ->
            case parseMany p remaining of
                Just (results, finalRemaining) -> Just (result:results, finalRemaining)
        _ -> Just ([], input)

parseSome :: Parser a -> Parser [a]
parseSome p =
    parseAndWith (\x xs -> x:xs) p (parseMany p)

