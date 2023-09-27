module Types (Parser) where


type Parser a = String -> Maybe (a, String)
