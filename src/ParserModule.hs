module ParserModule
    ( Parser(..)
    ) where

import Control.Applicative (Alternative(..))

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

-- >>= est utilisé pour séquencer deux actions, où la sortie de la première action détermine la seconde action à exécuter
instance Monad Parser where
    return = pure
    p >>= f = Parser $ \input ->
        case runParser p input of
            Nothing -> Nothing
            Just (a, remaining) -> runParser (f a) remaining

instance MonadFail Parser where
    fail msg = Parser $ \_ -> error msg
