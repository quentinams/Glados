import Control.Exception
import Datas
import Env
import Evaluation
-- import Control.Monad (foldM)

import Control.Applicative (Alternative(..))
-- import Control.Applicative ((<*>))
import Control.Monad (void)

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

-- <$> = fmap map d'un parseur à la valeur qui est dans le parseur
-- Symbol <$> parseSome utilise fmap pour construire un Symbol à partir du résultat de parseSome.
instance Functor Parser where
    fmap f p = Parser $ \s -> case runParser p s of
        Just (a, rest) -> Just (f a, rest)
        Nothing -> Nothing


-- <*> combine deux parseurs 1: produit une fonction 2: produit une valeur
-- pure place une valeur dans un contexte minimal
instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)

    p1 <*> p2 = Parser $ \input ->
        case runParser p1 input of
            Nothing -> Nothing
            Just (f, remaining1) ->
                case runParser p2 remaining1 of
                    Nothing -> Nothing
                    Just (a, remaining2) -> Just (f a, remaining2)

--tente d'abord le premier parseur si il echoue fait le deuxieme  <|>
-- empty parseur qui echoue toujours
instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    p1 <|> p2 = Parser $ \input ->
        case runParser p1 input of
            Just _  -> runParser p1 input
            Nothing -> runParser p2 input

-- >>= est utilisé pour séquencer deux actions, où la sortie de la première action détermine la seconde action à exécuter.

instance Monad Parser where
    return = pure
    p >>= f = Parser $ \input ->
        case runParser p input of
            Nothing -> Nothing
            Just (a, remaining) -> runParser (f a) remaining

instance MonadFail Parser where
    fail msg = Parser $ \_ -> error msg

parseChar :: Char -> Parser Char
parseChar c = Parser $ \s -> case s of
    (x:xs) -> if x == c then Just (c, xs) else Nothing
    [] -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar chars = Parser $ \input -> case input of
    (x:xs) | x `elem` chars -> Just (x, xs)
    _ -> Nothing

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

parseSymbol :: Parser Expr
parseSymbol = do
    skipSpaces
    Symbol <$> parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9' ] ++ "-_+*/"))

parseNumber :: Parser Expr
parseNumber = do
    skipSpaces
    numStr <- parseSome (parseAnyChar ['0'..'9'])
    return $ Number (read numStr :: Float)


parseBool :: Parser Expr
parseBool = do
    skipSpaces
    (Bool True <$ (parseChar '#' *> parseChar 't'))
    <|> (Bool False <$ (parseChar '#' *> parseChar 'f'))

parseList :: Parser Expr
parseList = do
    skipSpaces
    _ <- parseChar '('
    exprs <- parseMany (skipSpaces *> parseExpr <* skipSpaces)
    _ <- parseChar ')'
    return $ List exprs


parseLambda :: Parser Expr
parseLambda = do
    skipSpaces
    _ <- parseChar '('
    _ <- parseChar '\\'
    _ <- parseChar '('
    params <- parseMany (do
        skipSpaces
        Symbol sym <- parseSymbol
        return sym)
    _ <- parseChar ')'
    skipSpaces
    body <- parseExpr
    _ <- parseChar ')'
    return (Lambda params body)

parseFunc :: Parser Expr
parseFunc = do
    skipSpaces
    _ <- parseString "Function"
    skipSpaces
    _ <- parseChar '('
    params <- parseMany (do
        skipSpaces
        Symbol sym <- parseSymbol
        return sym)
    _ <- parseChar ')'
    skipSpaces
    body <- parseExpr
    return (Func params body)

-- Un parseur qui reconnaît n'importe quel type d'Expr
parseExpr :: Parser Expr
parseExpr = parseNumber
        <|> parseSymbol
        <|> parseBool
        <|> parseList
        <|> parseLambda
        <|> parseFunc


-- Utilitaire pour parser une chaîne exacte
parseString :: String -> Parser String
parseString = traverse (parseAnyChar . pure)

skipSpaces :: Parser ()
skipSpaces = void $ parseMany (parseAnyChar " \t\n\r")

parseExprs :: Parser [Expr]
parseExprs = do
    skipSpaces
    e <- parseExpr
    skipSpaces
    es <- (parseExprs <|> pure [])
    return (e:es)


main :: IO ()
main = loop initialEnv
  where
    loop env = do
        putStrLn "Enter your lisp expression (or type 'quit' to exit):"
        input <- getLine
        if input == "quit"
            then putStrLn "Goodbye!"
            else do
                case runParser parseExpr input of
                    Just (expr, _) -> do
                        (newEnv, maybeResult) <- evalAndStore (env, Nothing) expr
                        case maybeResult of
                            Just lastResult -> putStrLn $ show lastResult
                            Nothing -> putStrLn "No result to show."
                        loop newEnv
                    Nothing -> putStrLn "Failed to parse the lisp expression."



evalAndStore :: (Env, Maybe Expr) -> Expr -> IO (Env, Maybe Expr)
evalAndStore (env, _) expr = do
    result <- try (evaluate (evalExpr env expr)) :: IO (Either SomeException (Either String (Env, Expr)))
    case result of
        Left ex -> do 
            putStrLn $ "An error occurred: " ++ show ex
            return (env, Nothing)
        Right val ->
            case val of
                Left err -> do 
                    putStrLn $ "Evaluation error: " ++ err
                    return (env, Nothing)  -- Ici, nous renvoyons toujours l'ancien env
                Right (newEnv, res) -> do
                    return (newEnv, Just res)  -- Utilisez newEnv ici

