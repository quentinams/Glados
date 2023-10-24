module ASTConversion (exprToAST) where

import Datas (Expr(..), AST(..))

-- Fonction de conversion
exprToAST :: Expr -> AST
exprToAST (Symbol s)       = Var s
exprToAST (Number n)       = Const n
exprToAST (Bool b)         = TruthValue b
exprToAST (List (Symbol "if" : condition : consequent : [alternative])) =
    If (exprToAST condition) (exprToAST consequent) (exprToAST alternative)
exprToAST (List xs) = Application (exprToAST $ head xs) (map exprToAST $ tail xs)
exprToAST (Lambda params body) = LambdaFunc params (exprToAST body)
exprToAST (Func params body) = UserFunc params (exprToAST body)