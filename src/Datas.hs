module Datas (Expr(..), AST(..)) where

data Expr = Symbol String 
          | Number Float 
          | Bool Bool 
          | List [Expr] 
          | Lambda [String] Expr 
          | Func [String] Expr
          | Loop Expr Expr
          deriving (Eq)

instance Show Expr where
    show (Symbol s) = show s
    show (Number n) = if fromIntegral (floor n) == n then show (floor n) else show n
    show (Bool b) = if b then "#t" else "#f"
    show (List l) = show l
    show (Lambda params body) = "#<procedure>"
    show (Func params body) = "#<procedure>"

data AST = Var String
        | Const Float
        | TruthValue Bool
        | If AST AST AST
        | Sequence [AST]
        | LambdaFunc [String] AST
        | UserFunc [String] AST
        | Application AST [AST]
        | Definition String AST
        | Add AST AST
        | Sub AST AST
        | While AST AST
        | Eq AST AST
        | Assign String AST
        deriving (Eq, Show)
