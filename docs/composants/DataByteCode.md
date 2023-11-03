---
sidebar: auto
---

# DataByteCode.hs

## Module : `DataByteCode`

Ce module définie les types de base utilisés pour le byte code et offre également une fonction pour afficher une liste d'instructions.

### Types Principaux

#### Value

Un type pour représenter une valeur numérique ou booléenne.

```haskell
data Value = Num Int | Bool Bool deriving (Eq)
Num Int: Représente une valeur numérique.
Bool Bool: Représente une valeur booléenne.
Affichage
instance Show Value where
    show (Num n) = show n
    show (Bool b) = if b then "#t" else "#f"

```

Un type pour représenter différentes opérations.

```haskell
data Op = Add | Sub | Mul | Div | Eq | Less deriving (Show, Eq)
```

Représente différentes instructions bytecode.

```haskell
data Instruction = Push Value 
                 | Call Op 
                 | Ret 
                 | JumpIfFalse Int 
                 | Jump Int 
                 | Store String
                 | Load String
                 deriving (Show, Eq)
```
Types Associés

Stack: Une pile pour les Value.

```haskell
type Stack = [Value]
```
Insts: Une liste d'instructions.
```haskell
type Insts = [Instruction]
```

SymbolTable: Une table des symboles.
```haskell
type SymbolTable = [(String, Value)]
```

Fonction : showInstructions
Affiche une liste d'instructions.
```haskell
showInstructions :: Insts -> String
```

Description: Convertit une liste d'instructions en une chaîne de caractères.

```haskell
showInstructions insts = unlines (map show insts)
```

::: tip Note
Cette documentation donne un aperçu général des types et fonctions définis dans le module DataByteCode. Pour une compréhension plus détaillée, il est recommandé de consulter directement le code source.
:::