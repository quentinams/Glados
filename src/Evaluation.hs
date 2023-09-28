module Evaluation (evalExpr) where

import Env
import Datas

eval :: Env -> Expr -> Expr
eval env  (Symbol s) = 
    case lookupEnv s env of
        Just expr -> expr
        Nothing   -> error $ "Variable not found: " ++ s 
eval _ (Number n) = Number n
eval env (List(x:xs)) = apply env x xs

apply :: Env -> Expr -> [Expr] -> Expr
apply env (Symbol s) args = case s of
    "+" -> Number $ sum $ map evalInt args
    _   -> error $ "Unknown function: " ++ s
  where
    evalInt expr = case eval env expr of
        Number n -> n
        _        -> error "Expected number"
apply _ _ _ = error "Expected symbol at head of list"

evalExpr :: Expr -> Expr
evalExpr = eval initialEnv