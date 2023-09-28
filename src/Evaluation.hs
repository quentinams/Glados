module Evaluation (evalExpr) where

import Env
import Datas

eval :: Env -> Expr -> Expr
eval env (Symbol s) = 
    case lookupEnv s env of
        Just expr -> expr
        Nothing   -> error $ "Variable not found: " ++ s 
eval _ (Number n) = Number n
eval env (List(x:xs)) = apply env x xs

evalFloat :: Env -> Expr -> Float
evalFloat env expr = case eval env expr of
    Number n -> n
    _        -> error "Expected number"

apply :: Env -> Expr -> [Expr] -> Expr
apply env (Symbol s) args = case s of
    "+" -> addArgs env args
    "-" -> subtractArgs env args
    "*" -> multiplyArgs env args
    "/" -> divideArgs env args
    _   -> error $ "Unknown function: " ++ s
apply _ _ _ = error "Expected symbol at head of list"

addArgs :: Env -> [Expr] -> Expr
addArgs env args = Number $ foldl1 (+) (map (evalFloat env) args)

subtractArgs :: Env -> [Expr] -> Expr
subtractArgs env args = Number $ foldl1 (-) (map (evalFloat env) args)

multiplyArgs :: Env -> [Expr] -> Expr
multiplyArgs env args = Number $ foldl1 (*) (map (evalFloat env) args)

divideArgs :: Env -> [Expr] -> Expr
divideArgs env args = Number $ foldl1 (/) (map (evalFloat env) args)

evalExpr :: Expr -> Expr
evalExpr = eval initialEnv