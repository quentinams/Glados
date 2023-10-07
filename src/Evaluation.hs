module Evaluation (evalExpr) where

import Control.Monad (liftM2)
import Env
import Datas
import Debug.Trace
import MathFunctions

getName :: Expr -> Either String String
getName (Symbol s) = Right s
getName _ = Left "Expected a symbol"

eval :: Env -> Expr -> Either String (Env, Expr)
eval env (List ((Symbol "lambda"):List params:body:[])) = 
    let strParams = map (\(Symbol s) -> s) params in
    Right (env, Lambda strParams body)
eval env (List (f:args)) = 
    case eval env f of
        Right (_, Lambda params body) -> 
            let argVals = map (\arg -> case eval env arg of 
                                            Right (_, val) -> val
                                            Left err -> Symbol ("Error: " ++ err)) args
                newEnv = zip params argVals ++ env
            in eval newEnv body
        Right (_, f') -> apply env f' args
        Left err -> Left err
eval env expr = 
    case expr of
        Symbol s -> 
            case lookupEnv s env of
                Just expr' -> Right (env, expr')
                Nothing    -> Left $ "Variable not found: " ++ s 
        Number n -> Right (env, Number n)
        List(x:xs) -> apply env x xs

apply :: Env -> Expr -> [Expr] -> Either String (Env, Expr)
apply env (Symbol s) args = 
   case s of
        "define" -> defineVar env args
        "+" -> addArgs env args
        "div" -> divideArgs env args
        "mod" -> moduloArgs env args
        "eq?" -> equalExpr env args
        "<" -> lessThanExpr env args
        _   -> Left $ "Unknown function: " ++ s
apply _ _ _ = Left "Expected symbol at head of list"

equalExpr :: Env -> [Expr] -> Either String (Env, Expr)
equalExpr env [expr1, expr2] = 
    do (env1, val1) <- eval env expr1
       (env2, val2) <- eval env1 expr2
       return (env2, Bool $ val1 == val2)
equalExpr _ _ = Left "eq? expects exactly two arguments"

lessThanExpr :: Env -> [Expr] -> Either String (Env, Expr)
lessThanExpr env [expr1, expr2] = 
    do (env1, val1) <- eval env expr1
       (env2, val2) <- eval env1 expr2
       case (val1, val2) of
         (Number n1, Number n2) -> return (env2, Bool $ n1 < n2)
         _                      -> Left "lessThan: expects two numbers"
lessThanExpr _ _ = Left "lessThan: expects exactly two arguments"

defineVar :: Env -> [Expr] -> Either String (Env, Expr)
defineVar env [Symbol var, expr] = 
    eval env expr >>= \(newEnv, val) -> Right (extendEnv newEnv [(var, val)], val)
defineVar _ _ = Left "define expects a symbol and an expression"

addArgs :: Env -> [Expr] -> Either String (Env, Expr)
addArgs env args = 
    fmap (\nums -> (env, Number $ foldl (+) 0 nums)) (mapM (evalFloat env) args)

evalExpr :: Env -> Expr -> Either String (Env, Expr)
evalExpr = eval
