module Evaluation (evalExpr) where

import Env
import Datas
import Debug.Trace

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

evalFloat :: Env -> Expr -> Either String Float
evalFloat env (Symbol s) = 
    case lookupEnv s env of
        Just (Number n) -> Right n
        _               -> Left $ "Expected number for symbol: " ++ s
evalFloat env (Number n) = Right n
evalFloat _ _ = Left "Expected number"

apply :: Env -> Expr -> [Expr] -> Either String (Env, Expr)
apply env (Symbol s) args = 
   case s of
        "define" -> defineVar env args
        "+" -> addArgs env args
        "div" -> divideArgs env args
        "mod" -> moduloArgs env args
        _   -> Left $ "Unknown function: " ++ s
apply _ _ _ = Left "Expected symbol at head of list"

defineVar :: Env -> [Expr] -> Either String (Env, Expr)
defineVar env [Symbol var, expr] = 
    do
        (newEnv, val) <- eval env expr
        Right (extendEnv newEnv [(var, val)], val)
defineVar _ _ = Left "define expects a symbol and an expression"

addArgs :: Env -> [Expr] -> Either String (Env, Expr)
addArgs env args = 
    do
        nums <- mapM (evalFloat env) args
        return (env, Number $ foldl (+) 0 nums)

subtractArgs :: Env -> [Expr] -> Either String (Env, Expr)
subtractArgs env args = 
    do
        nums <- mapM (evalFloat env) args
        Right (env, Number $ foldl1 (-) nums)

multiplyArgs :: Env -> [Expr] -> Either String (Env, Expr)
multiplyArgs env args = 
    do
        nums <- mapM (evalFloat env) args
        Right (env, Number $ foldl1 (*) nums)

divideArgs :: Env -> [Expr] -> Either String (Env, Expr)
divideArgs env args = 
    do
        nums <- mapM (evalFloat env) args
        if any (== 0) nums
            then Left "Division by zero"
            else Right (env, Number $ foldl1 (/) nums)

moduloArgs :: Env -> [Expr] -> Either String (Env, Expr)
moduloArgs env args 
    | length args /= 2 = Left "'mod' expects exactly two arguments"
    | otherwise = 
        do
            n1 <- round <$> evalFloat env (head args)
            n2 <- round <$> evalFloat env (args !! 1)
            if n2 == 0 
            then Left "Division by zero in 'mod'"
            else Right (env, Number (fromIntegral $ n1 `mod` n2))

equalExpr :: Env -> Expr -> Expr -> Either String Bool
equalExpr env (Number n1) (Number n2) = Right (n1 == n2)
equalExpr env (Symbol s1) (Symbol s2) = Right (s1 == s2)
equalExpr env (List l1) (List l2) = 
    if length l1 == length l2
    then fmap and (sequence (zipWith (equalExpr env) l1 l2))
    else Left "Lists do not have the same length"
equalExpr _ _ _ = Left "Mismatched types"

compareExprs :: Env -> Expr -> Expr -> Either String (Maybe Ordering)
compareExprs env (Number n1) (Number n2) = Right $ Just $ compare n1 n2
compareExprs env (Symbol s1) (Symbol s2) = Right $ Just $ compare s1 s2
compareExprs env (List l1) (List l2) = 
    if length l1 == length l2
    then fmap (foldl combine (Just EQ)) (sequence (zipWith (compareExprs env) l1 l2))
    else Left "Lists do not have the same length"
  where
    combine (Just EQ) o = o
    combine _ _ = Nothing
compareExprs _ _ _ = Left "Mismatched types"

evalExpr :: Env -> Expr -> Either String (Env, Expr)
evalExpr = eval
