# Configuration du projet Glados

Ce document guide les développeurs à travers les étapes nécessaires pour configurer et démarrer le projet **Lisp Interpreter** sur leur machine locale.

## Prérequis

- Haskell : Le projet est écrit en Haskell, assurez-vous d'avoir une version récente de GHC (Glasgow Haskell Compiler) et Cabal (un outil de construction et de gestion de paquet pour Haskell).

### Étapes de configuration

- #### 1. Cloner le dépôt :
Commencez par cloner le dépôt sur votre machine locale.
```
git clone [url-du-dépôt]
```

- #### 2. Installer les dépendances :
Naviguez vers le dossier du projet et utilisez cabal pour installer les dépendances.
```
cd chemin/vers/le/projet
cabal install
```

- #### 3. Compilation :

Compilez le projet pour vous assurer que tout est en ordre.
```
cabal build
```

- #### 4. Exécution :

Une fois compilé, vous pouvez exécuter le projet :
```
cabal run
```

## Test
Pour vous assurer que tout fonctionne comme prévu :

```
cabal test
```
