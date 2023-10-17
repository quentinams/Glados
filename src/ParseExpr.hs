module ParseExpr (parseList, parseExpr, parseExprs) where


import ParserModule
import Datas
import ParseChar
import ParseAnd
import ParseUtils

import Control.Applicative ((<|>))

parseAlpha :: Parser Char
parseAlpha = parseAnyChar ['a'..'z'] <|> parseAnyChar ['A'..'Z']

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

parseVariable :: Parser String
parseVariable = do
    first <- parseAlpha
    rest <- parseMany (parseAlpha <|> parseDigit)  -- On accepte des chiffres après le premier caractère.
    return (first:rest)

parseListWith :: Parser a -> Parser [a]
parseListWith p = do
    skipSpaces
    _ <- parseChar '('
    items <- parseMany (skipSpaces *> p <* skipSpaces)
    _ <- parseChar ')'
    return items

parseLambda :: Parser Expr
parseLambda = do
  _ <- parseString "("
  _ <- skipSpaces
  _ <- parseString "lambda"
  _ <- skipSpaces
  params <- parseListWith parseVariable
  _ <- skipSpaces
  body <- parseExpr
  _ <- skipSpaces
  _ <- parseString ")"
  return $ Lambda params body



-- parseFunc :: Parser Expr
-- parseFunc = do
--     skipSpaces
--     _ <- parseString "Function"
--     skipSpaces
--     _ <- parseChar '('
--     params <- parseMany (do
--         skipSpaces
--         Symbol sym <- parseSymbol
--         return sym)
--     _ <- parseChar ')'
--     skipSpaces
--     body <- parseExpr
--     return (Func params body)

-- parseNamedFunctionDefinition :: Parser Expr
-- parseNamedFunctionDefinition = do
--     skipSpaces
--     _ <- parseString "define"
--     skipSpaces
--     _ <- parseChar '('
--     Symbol funcName <- parseSymbol
--     skipSpaces
--     args <- parseMany (skipSpaces *> parseSymbol)
--     skipSpaces
--     _ <- parseChar ')'
--     body <- parseExpr
--     let lambdaExpr = Lambda (map (\(Symbol s) -> s) args) body
--     return $ List [Symbol "define", Symbol funcName, lambdaExpr]


-- parseDefine :: Parser Expr
-- parseDefine = do
--     skipSpaces
--     _ <- parseString "define"
--     skipSpaces
--     _ <- parseChar '('
--     Symbol funcName <- parseSymbol
--     params <- parseMany (do
--         skipSpaces
--         Symbol sym <- parseSymbol
--         return sym)
--     _ <- parseChar ')'
--     skipSpaces
--     body <- parseExpr
--     return (Define funcName params body)

parseList :: Parser Expr
parseList = do
    skipSpaces
    _ <- parseChar '('
    exprs <- parseMany (skipSpaces *> parseExpr <* skipSpaces)
    _ <- parseChar ')'
    return $ List exprs

-- Un parseur qui reconnaît n'importe quel type d'Expr
parseExpr :: Parser Expr
parseExpr = parseNumber  
        -- <|> parseDefine
        -- <|> parseNamedFunctionDefinition
        <|> parseSymbol
        <|> parseBool
        <|> parseList
        <|> parseLambda
        -- <|> parseFunc

parseExprs :: Parser [Expr]
parseExprs = do
    skipSpaces
    e <- parseExpr
    skipSpaces
    es <- (parseExprs <|> pure [])
    return (e:es)