---
sidebar: auto
---

# Writebytecode.hs

## Module : `WriteByteCode`

Le module `WriteByteCode` est responsable de la compilation des expressions `AST` en bytecode pour être exécuté par la machine virtuelle.

### Fonction principale

#### `compile :: AST -> Either String Insts`

Compile une expression AST en une liste d'instructions bytecode.

- Si la compilation est réussie, elle renvoie un `Right` contenant les instructions.
- Si une erreur survient, elle renvoie un `Left` avec un message d'erreur.

### Fonctions auxiliaires

- **`compileIf`** : Compile une expression conditionnelle `If`.
  
- **`compileSequence`** : Compile une séquence d'expressions AST.

- **`compileBinaryOp`** : Compile une opération binaire (comme l'addition, la soustraction, etc.).

- **`compileDefinition`** : Compile une définition de variable.

- **`compileVar`** : Compile une référence à une variable.

### Détails d'implémentation

- La fonction `compile` gère différentes formes d'AST, allant des valeurs primitives (comme les nombres et les booléens) aux structures plus complexes comme les séquences et les opérations conditionnelles.

- Les fonctions `compileIf` et `compileSequence` utilisent la monade `Either` pour chaîner les opérations de compilation, ce qui permet de propager facilement les erreurs.

- La fonction `compileBinaryOp` généralise la compilation de toutes les opérations binaires.

::: tip Note
Bien que le module gère actuellement un sous-ensemble des expressions Lisp, il peut être étendu pour gérer davantage de fonctionnalités au fur et à mesure que le langage évolue.
:::
