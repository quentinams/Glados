---
sidebar: auto
---

# ParseOr.hs

## Module : `ParseOr`

Ce module fournit des fonctions pour parser en utilisant l'opération logique OR. C'est utile lorsque vous voulez tenter d'utiliser plusieurs parseurs sur une entrée et prendre le premier qui réussit.

### Fonctions Principales

#### parseOr

Tente de parser avec `p1`, et si cela échoue, il essaie `p2`.

```haskell
parseOr :: Parser a -> Parser a -> Parser a
```
#### parseAnyCharUsingOr
Tente de parser n'importe quel caractère de la chaîne chars.

```haskell
parseAnyCharUsingOr :: String -> Parser Char
```

- Si un des caractères est trouvé, il renvoie ce caractère et le reste de l'entrée.
- Sinon, il renvoie Nothing.

### Fonctions auxiliaires

#### filterChars

Une fonction qui tente de trouver le premier caractère de la liste **chars** dans la chaîne **input**.
```haskell
filterChars :: String -> String -> Maybe (Char, String)
```

### Détails d'implémentation

- La principale logique du module repose sur la fonction **parseOr**, qui utilise le résultat du premier parseur si celui-ci réussit, sinon elle utilise le deuxième parseur.
- **filterChars** est utilisé dans **parseAnyCharUsingOr** pour filtrer et récupérer le premier caractère correspondant de l'entrée.

::: tip Note
Cette documentation donne un aperçu général des fonctions définies dans le module ParseOr. Pour une compréhension plus détaillée, il est recommandé de consulter directement le code source.
:::