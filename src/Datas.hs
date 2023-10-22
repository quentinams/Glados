module Datas (Expr(..)) where

data Expr = Symbol String 
          | Number Float 
          | Bool Bool 
          | List [Expr] 
          | Lambda [String] Expr 
          | Func [String] Expr
          deriving (Eq)

instance Show Expr where
    show (Symbol s) = show s
    show (Number n) = if fromIntegral (floor n) == n then show (floor n) else show n
    show (Bool b) = if b then "#t" else "#f"
    show (List l) = show l
    show (Lambda params body) = "#<procedure>"
    show (Func params body) = "#<procedure>"
