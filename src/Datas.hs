module Datas (Expr(..)) where

data Expr = Symbol String | Number Float | List [Expr] | Lambda [String] Expr | Func [String] Expr deriving (Show)
