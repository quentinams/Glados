module WriteByteCode (compile) where

import Datas (Expr(..), AST(..))
import DataByteCode (Value(..), Op(..), Instruction(..), Insts)

-- Compile une expression AST en bytecode
compile :: AST -> Either String Insts
compile ast = 
    case ast of
        Var x -> Left $ "Unsupported operation: variables should be resolved before this stage. Found: " ++ x
        Const n -> Right [Push (Num (round n))]  -- conversion Float vers Int, attention à la perte de précision
        TruthValue b -> Right [Push (DataByteCode.Bool b)]
        If cond thenBranch elseBranch -> compileIf cond thenBranch elseBranch
        Sequence exprs -> compileSequence exprs
        -- Les autres cas nécessitent une gestion plus complexe des environnements, etc.
        _ -> Left $ "Unsupported operation during compilation: " ++ show ast

-- Compile un AST 'If'
compileIf :: AST -> AST -> AST -> Either String Insts
compileIf cond thenBranch elseBranch = do
    condCode <- compile cond
    thenCode <- compile thenBranch
    elseCode <- compile elseBranch
    let jumpOverThen = [JumpIfFalse (length thenCode + 2)]  -- +2 pour sauter l'instruction 'Jump' et tout le bloc 'else'
    let jumpToEnd = [Jump (length elseCode)]  -- saut inconditionnel à la fin du 'then'
    return $ condCode ++ jumpOverThen ++ thenCode ++ jumpToEnd ++ elseCode


-- Compile un AST 'Sequence'
compileSequence :: [AST] -> Either String Insts
compileSequence [] = Right []
compileSequence (x:xs) = do
    xCode <- compile x
    xsCode <- compileSequence xs
    return $ xCode ++ xsCode

-- ... Vous devrez implémenter le reste des cas en suivant une logique similaire.
