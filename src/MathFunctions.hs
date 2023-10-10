module MathFunctions (subtractArgs, multiplyArgs, floatDivision, euclideanDivision, moduloArgs, evalFloat) where

import Datas
import Types
import Env

subtractArgs :: Env -> [Expr] -> Either String (Env, Expr)
subtractArgs env args = 
    fmap (\nums -> (env, Number $ foldl1 (-) nums)) (mapM (evalFloat env) args)

multiplyArgs :: Env -> [Expr] -> Either String (Env, Expr)
multiplyArgs env args = 
    fmap (\nums -> (env, Number $ foldl1 (*) nums)) (mapM (evalFloat env) args)

floatDivision :: Env -> [Expr] -> Either String (Env, Expr)
floatDivision env args = 
    mapM (evalFloat env) args >>= \nums ->
    if any (== 0) nums
        then Left "Division by zero"
        else Right (env, Number $ foldl1 (/) nums)

euclideanDivision :: Env -> [Expr] -> Either String (Env, Expr)
euclideanDivision env args 
    | length args /= 2 = Left "'div' expects exactly two arguments"
    | otherwise = 
        do
            n1 <- round <$> evalFloat env (head args)
            n2 <- round <$> evalFloat env (args !! 1)
            if n2 == 0 
            then Left "Division by zero in 'div'"
            else Right (env, Number (fromIntegral $ n1 `div` n2))

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

evalFloat :: Env -> Expr -> Either String Float
evalFloat env (Symbol s) = 
    case lookupEnv s env of
        Just (Number n) -> Right n
        _               -> Left $ "Expected number for symbol: " ++ s
evalFloat env (Number n) = Right n
evalFloat _ _ = Left "Expected number"
