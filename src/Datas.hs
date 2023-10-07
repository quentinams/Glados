module Datas (Expr(..)) where

data Expr = Symbol String | Number Float | Bool Bool | List [Expr] | Lambda [String] Expr | Func [String] Expr deriving (Show, Eq)
