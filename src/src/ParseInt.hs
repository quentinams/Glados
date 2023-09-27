module ParseInt (parseUInt, parseInt) where
import Data.Char (digitToInt)
import ParseAnd
import ParseOr
import ParseChar
import Types

parseUInt :: Parser Int
parseUInt = parseAndWith (\digits _ -> foldl (\acc d -> acc * 10 + digitToInt d) 0 digits) (parseSome (parseAnyChar ['0'..'9'])) (parseMany (parseChar ' '))

parseInt :: Parser Int
parseInt = parseOr (parseAndWith (\sign num -> if sign == '-' then -num else num) (parseAnyChar "+-") parseUInt) parseUInt


