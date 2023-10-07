module Datas (Expr(..)) where

data Expr = Symbol String | Number Float | Bool Bool | List [Expr] | Lambda [String] Expr | Func [String] Expr deriving (Eq)

instance Show Expr where
    show (Symbol s) = show s
    show (Number n) = show n
    show (Bool b) = show b
    show (List l) = show l
    show (Lambda params body) = "Lambda " ++ show params ++ " " ++ show body
    show (Func params body) = "Func " ++ show params
