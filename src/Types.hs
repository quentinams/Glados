module Types (Parser) where
import Datas

type Parser a = String -> Maybe (a, String)