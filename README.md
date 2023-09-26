# Projet Haskell: Compilateur Lisp

## Description

Ce projet est un compilateur Lisp écrit en Haskell. Le code source est organisé conformément aux bonnes pratiques de programmation en Haskell et utilise `stack` pour la gestion des dépendances et la construction du projet.

## Prérequis

- Assurez-vous d'avoir installé [Stack](https://docs.haskellstack.org/en/stable/README/) sur votre machine.

## Configuration de l'environnement de développement

1. Avant de déveloper il faut build le projet :
   ```bash 
    make build
    ```

## Comment coder sur le projet

### Execution des tests : 

Les tests sont dans le dossier test, pour le projet on utilise [HUnit](https://hackage.haskell.org/package/HUnit) :

Pour lancer les tests faites : 
 ```bash
    make run-test
```

### Exection simple : 

Pour juste lancer le `Main.hs` dans le dossier app :

 ```bash
    make
```

### Où coder :

Vosu devez coder dans le dossier src chaque fichier sera un module vous pouvez regarder le fichier Lib.hs pour prendre exemple sur comment il faut importer vos fonctions.

## Annexe

Vous pouvez bien sur clean, fclean et re le projet.