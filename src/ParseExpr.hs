module ParseExpr (parseList, parseExpr, parseExprs, parseLispFile) where


import ParserModule
import Datas
import ParseChar
import ParseAnd
import ParseUtils
import ASTConversion (exprToAST)
import WriteByteCode (compile)
import EvalByteCode (exec)

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

parseEq :: Parser Expr
parseEq = do
    skipSpaces
    _ <- parseString "("
    _ <- skipSpaces
    _ <- parseString "eq?"
    _ <- skipSpaces
    arg1 <- parseExpr
    skipSpaces
    arg2 <- parseExpr
    _ <- skipSpaces
    _ <- parseString ")"
    return $ List [Symbol "eq?", arg1, arg2]

parseList :: Parser Expr
parseList = do
    skipSpaces
    _ <- parseChar '('
    exprs <- parseMany (skipSpaces *> parseExpr <* skipSpaces)
    _ <- parseChar ')'
    return $ List exprs

parseLoop :: Parser Expr
parseLoop = do
    skipSpaces
    _ <- parseString "("
    _ <- skipSpaces
    _ <- parseString "loop"
    _ <- skipSpaces
    condition <- parseExpr
    skipSpaces
    body <- parseExpr
    _ <- skipSpaces
    _ <- parseString ")"
    return $ Loop condition body


-- Un parseur qui reconnaît n'importe quel type d'Expr
parseExpr :: Parser Expr
parseExpr = parseNumber
        <|> parseEq
        -- <|> parseNamedFunctionDefinition
        <|> parseSymbol
        <|> parseBool
        <|> parseList
        <|> parseLambda
        <|> parseLoop

-- parseExprs :: Parser [Expr]
-- parseExprs = do
--     skipSpaces
--     e <- parseExpr
--     skipSpaces
--     es <- (parseExprs <|> pure [])
--     return (e:es)

-- Ajoutez cette fonction pour analyser plusieurs expressions dans un fichier.
parseLispFile :: String -> Either String [Expr]
parseLispFile content = 
    case runParser parseExprs content of
        Just (exprs, _) -> Right exprs
        Nothing -> Left "Failed to parse the Lisp file."

-- Modifiez votre fonction `processLisp` pour qu'elle utilise la nouvelle fonction.
processLisp :: String -> IO ()
processLisp content = 
    case parseLispFile content of
        Right exprs -> do
            let asts = map exprToAST exprs
            case compile (Sequence asts) of
                Left err -> putStrLn $ "Compilation error: " ++ err
                Right bytecode -> do
                    case exec bytecode [] [] of
                        Left runtimeErr -> putStrLn $ "Runtime error: " ++ runtimeErr
                        Right value -> putStrLn $ show value
        Left err -> putStrLn err

-- Parse file
parseExprs :: Parser [Expr]
parseExprs = do
    skipSpaces
    es <- parseMany (skipSpaces *> parseExpr <* skipSpaces)
    return es