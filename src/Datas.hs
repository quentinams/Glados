module Datas (Expr(..)) where

data Expr = Symbol String | Number Int | List [Expr] deriving (Show)
