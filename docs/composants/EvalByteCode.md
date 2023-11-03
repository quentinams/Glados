---
sidebar: auto
---

# EvalByteCode.hs

## Module : `EvalByteCode`

Ce module définit les fonctions d'exécution pour le byte code défini dans le module `DataByteCode`.

### Fonctions Principales

#### execOp

Évalue une opération sur une pile de valeurs.

```haskell
execOp :: Op -> Stack -> Either String Stack
```
Reçoit une opération et une pile.
Renvoie le résultat de l'opération ou une erreur.
exec
Exécute une liste d'instructions byte code.

```haskell
exec :: Insts -> Stack -> SymbolTable -> Either String (Value, SymbolTable)
```
Reçoit une liste d'instructions, une pile initiale et une table des symboles.
Renvoie une valeur et une table des symboles mises à jour ou une erreur.

Détails d'implémentation
La fonction execOp gère les opérations arithmétiques et logiques. Elle gère également les erreurs comme la division par zéro.

La fonction exec est une fonction récursive qui parcourt la liste d'instructions. Elle gère diverses instructions comme Push, Call, JumpIfFalse, Jump, Load et Store.

::: tip Note
Cette documentation donne un aperçu général des fonctions définies dans le module EvalByteCode. Pour une compréhension plus détaillée, il est recommandé de consulter directement le code source.
:::