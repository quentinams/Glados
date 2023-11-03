---
sidebar: auto
---

# ParseExpr.hs

## Module : `ParseExpr`

Ce module fournit des fonctions pour parser les expressions Lisp, allant des expressions de base (comme les nombres et les symboles) aux expressions plus complexes (comme les fonctions Lambda et les boucles). Il s'appuie sur divers modules auxiliaires pour le parsing et la transformation en AST, puis en bytecode.

### Fonctions Principales

#### parseExpr

Parse n'importe quel type d'expression Lisp.

```haskell
parseExpr :: Parser Expr
```

#### parseLispFile
Parse plusieurs expressions Lisp à partir du contenu d'un fichier.
```haskell
parseLispFile :: String -> Either String [Expr]
```
Si le parsing réussit, il renvoie une liste d'expressions.
Si le parsing échoue, il renvoie une erreur sous forme de chaîne de caractères.

#### processLisp
Traite le contenu d'un fichier Lisp, le parse, le transforme en AST, le compile en bytecode, puis exécute le bytecode.
```haskell
processLisp :: String -> IO ()
```

### Autres fonctions de parsing
Ce module définit également d'autres fonctions de parsing pour gérer différentes expressions Lisp, telles que :

- **parseLambda**: Parse une expression lambda.

- **parseEq**: Parse une expression d'égalité.

- **parseList**: Parse une liste d'expressions.

- **parseLoop**: Parse une boucle.

### Détails d'implémentation
- Les parseurs combinent souvent plusieurs petits parseurs pour reconnaître des structures plus grandes. Par exemple, parseLambda utilise parseVariable pour parser les paramètres et parseExpr pour parser le corps de la fonction.
- L'utilisation de l'opérateur <|> permet de tenter plusieurs parseurs jusqu'à ce qu'un réussisse. Par exemple, parseExpr tente de parser un nombre, puis une égalité, un symbole, etc., jusqu'à ce qu'un parseur réussisse.