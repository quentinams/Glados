module ParseUtils ( parseString, skipSpaces, parseBool, parseSymbol, parseNumber) where

import ParserModule
import Datas
import ParseChar
import ParseAnd

import Control.Monad (void)
import Control.Applicative ((<|>))

parseString :: String -> Parser String
parseString = traverse (parseAnyChar . pure)

skipSpaces :: Parser ()
skipSpaces = void $ parseMany (parseAnyChar " \t\n\r")

parseBool :: Parser Expr
parseBool = skipSpaces *> ((Bool True <$ (parseChar '#' *> parseChar 't')) <|> (Bool False <$ (parseChar '#' *> parseChar 'f')))

parseSymbol :: Parser Expr
parseSymbol = Symbol <$> (skipSpaces *> parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9' ] ++ "-_+*/=<")))

parseNumber :: Parser Expr
parseNumber = do
    skipSpaces
    numStr <- parseSome (parseAnyChar ['0'..'9'])
    return $ Number (read numStr :: Float)
