module Env (Env, lookupEnv, initialEnv, extendEnv) where
import Datas
import Debug.Trace


type Env = [(String, Expr)]

initialEnv :: Env
initialEnv = [("+", Symbol "+"),
              ("-", Symbol "-"),
              ("*", Symbol "*"),
              ("div", Symbol "div"),
              ("mod", Symbol "mod"),
              (">", Symbol ">"),
              ("<", Symbol "<"),
              ("eq?", Symbol "eq?"),
              ("define", Symbol "define"),
              ("if", Symbol "if")]
              

lookupEnv :: String -> Env -> Maybe Expr
lookupEnv name env = lookupEnv' name env

lookupEnv' :: String -> Env -> Maybe Expr
lookupEnv' _ [] = Nothing
lookupEnv' name ((key, value):xs) =
    if name == key
        then Just value
        else lookupEnv' name xs

extendEnv :: Env -> [(String, Expr)] -> Env
extendEnv env newBindings = newBindings ++ env