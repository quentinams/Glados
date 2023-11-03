---
sidebar: auto
---

# Datas.hs

## Module : `Datas`

Ce module définit les types de base utilisés pour les expressions et l'arbre de syntaxe abstraite (AST) dans votre langage.

### Types Principaux

#### Expr

Un type pour représenter différentes formes d'expressions dans le langage.

```haskell
data Expr = Symbol String 
          | Number Float 
          | Bool Bool 
          | List [Expr] 
          | Lambda [String] Expr 
          | Func [String] Expr
          | Loop Expr Expr
          deriving (Eq)
          Symbol String : Représente une variable ou un identificateur.
```

Number Float : Représente une valeur numérique.

Bool Bool : Représente une valeur booléenne.

List [Expr] : Représente une liste d'expressions.

Lambda [String] Expr : Représente une fonction lambda.

Func [String] Expr : Représente une fonction définie par l'utilisateur.

Loop Expr Expr : Représente une boucle.

```haskell
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
        deriving (Eq, Show)
```

::: tip Note
Cette documentation donne un aperçu général des types définis dans le module Datas. Pour une compréhension plus détaillée, il est recommandé de consulter directement le code source.
:::