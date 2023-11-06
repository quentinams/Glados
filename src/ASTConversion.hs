module ASTConversion (exprToAST) where

import Datas (Expr(..), AST(..))

-- Fonction de conversion
exprToAST :: Expr -> AST
exprToAST (Symbol s)       = Var s
exprToAST (Number n)       = Const n
exprToAST (Bool b)         = TruthValue b
exprToAST (List (Symbol "if" : condition : consequent : [alternative])) =
    If (exprToAST condition) (exprToAST consequent) (exprToAST alternative)
exprToAST (List (Symbol "+" : left : [right])) = 
    Add (exprToAST left) (exprToAST right)
exprToAST (List (Symbol "-" : left : [right])) = 
    Sub (exprToAST left) (exprToAST right)
exprToAST (List (Symbol "eq?" : left : [right])) = 
    Eq (exprToAST left) (exprToAST right)
exprToAST (List(Symbol "=" : Symbol name : [value])) = 
    Assign name (exprToAST value)
exprToAST (List(Symbol "define" : Symbol name : [value])) = 
    Definition name (exprToAST value)
exprToAST (List xs) = Application (exprToAST $ head xs) (map exprToAST $ tail xs)
exprToAST (Lambda params body) = LambdaFunc params (exprToAST body)
exprToAST (Func params body) = UserFunc params (exprToAST body)