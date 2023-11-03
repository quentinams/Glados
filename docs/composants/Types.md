---
sidebar: auto
---

# Types.hs

## Module : `Types`

Le module `Types` est un petit module qui définit un type synonyme pour représenter le type de base du parseur.

### Type de base

#### `Parser`

`Parser` est un type synonyme qui prend un type générique `a` et retourne un type qui représente une fonction prenant une `String` et renvoyant peut-être un tuple contenant une valeur de type `a` et une `String` restante.

```haskell
type Parser a = String -> Maybe (a, String)
```

### Détails d'implémentation

- Ce type est la base de toutes les fonctions de parsing dans les autres modules.
- La chaîne entrante est celle que le parseur tente d'analyser.
- Le tuple renvoyé contient la partie analysée de la chaîne (de type **a**) et le reste de la chaîne qui n'a pas encore été analysé.
- La valeur **Maybe** permet de gérer l'échec du parsing : si le parsing échoue, **Nothing** est renvoyé ; sinon, **Just (valeur, reste)** est renvoyé.

::: tip Note
Ce module est essentiellement un support pour les autres modules de parsing. Il n'implémente pas de logique de parsing en soi, mais fournit le type de base utilisé par les autres parseurs.
:::