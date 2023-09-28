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
    "eq?" ->
        if length args == 2
           then if equalExpr env (head args) (args !! 1) 
                   then Symbol "true" 
                   else Symbol "false"
           else error "'eq?' expects exactly two arguments"
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

equalExpr :: Env -> Expr -> Expr -> Bool
equalExpr env (Number n1) (Number n2) = n1 == n2
equalExpr env (Symbol s1) (Symbol s2) = s1 == s2
equalExpr env (List l1) (List l2) = all (uncurry (equalExpr env)) (zip l1 l2)
equalExpr _ _ _ = False

evalExpr :: Expr -> Expr
evalExpr = eval initialEnv