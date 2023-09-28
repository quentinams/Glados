module Env (Env, lookupEnv, initialEnv) where
import Datas

type Env = [(String, Expr)]

initialEnv :: Env
initialEnv = [("+", Symbol "+"), ("-", Symbol "-"), ("*", Symbol "*"), ("/", Symbol "/")]

lookupEnv :: String -> Env -> Maybe Expr
lookupEnv _ [] = Nothing
lookupEnv name ((key, value):xs) =
    if name == key
        then Just value
        else lookupEnv name xs