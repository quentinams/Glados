# ASTConversion.hs

## Module : `ASTConversion`

Ce module fournit une fonction pour convertir une expression Lisp `Expr` en un Abstract Syntax Tree (`AST`). 

### Importations

- **`Datas`**: Ce module importe le type `Expr` et le type `AST` du module `Datas`.

## Fonction : `exprToAST`

### Type de Signature

```haskell
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
exprToAST (List(Symbol "define" : Symbol name : [value])) = 
    Definition name (exprToAST value)
exprToAST (List xs) = Application (exprToAST $ head xs) (map exprToAST $ tail xs)
exprToAST (Lambda params body) = LambdaFunc params (exprToAST body)
exprToAST (Func params body) = UserFunc params (exprToAST body)
```

Description

Convertit une expression Lisp Expr en un Abstract Syntax Tree (AST).

Arguments
Expr: Une expression Lisp qui peut être l'un des types suivants : Symbol, Number, Bool, List, Lambda, Func.
Retour
AST: L'Abstract Syntax Tree correspondant.
Exemples de Conversion
Symbol: Convertit un symbole en une variable Var.
```haskell
exprToAST (Symbol s) = Var s
```

Number: Convertit un nombre en une constante Const.
```haskell
exprToAST (Number n) = Const n
```

Bool: Convertit une valeur booléenne en une valeur de vérité TruthValue.
```haskell
exprToAST (Bool b) = TruthValue b
```

List (avec opérations spécifiques): Convertit une liste d'expressions représentant différentes opérations (par exemple, if, +, -, eq?, define) en leurs représentations AST correspondantes.
```haskell
exprToAST (List (Symbol "if" : condition : consequent : [alternative])) = 
    If (exprToAST condition) (exprToAST consequent) (exprToAST alternative)
```

Lambda: Convertit une lambda en une fonction lambda LambdaFunc.
```haskell
exprToAST (Lambda params body) = LambdaFunc params (exprToAST body)
```

Func: Convertit une fonction définie par l'utilisateur en une fonction UserFunc.
```haskell
exprToAST (Func params body) = UserFunc params (exprToAST body)
```

::: tip Note
Cette documentation donne un aperçu général de la fonctionnalité du module et comment il convertit différentes expressions Lisp en leurs équivalents AST. Pour une compréhension plus approfondie, il est recommandé de se référer directement au code source.
:::
