module ParseAnd (parseAnd, parseAndWith, parseMany, parseSome) where
import ParserModule (Parser(..))

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ \input ->
    case runParser p1 input of
        Nothing -> Nothing
        Just (result1, remaining1) ->
            case runParser p2 remaining1 of
                Nothing -> Nothing
                Just (result2, remaining2) -> Just ((result1, result2), remaining2)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser $ \input ->
    case runParser (parseAnd p1 p2) input of
        Nothing -> Nothing
        Just ((result1, result2), remaining) -> Just (f result1 result2, remaining)

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \input ->
    case runParser p input of
        Nothing -> Just ([], input)
        Just (result, remaining) ->
            case runParser (parseMany p) remaining of
                Nothing -> Just ([result], remaining)
                Just (results, finalRemaining) -> Just (result:results, finalRemaining)

parseSome :: Parser a -> Parser [a]
parseSome p =
    parseAndWith (\x xs -> x:xs) p (parseMany p)

