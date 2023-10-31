import subprocess
import os

import pytest

@pytest.fixture(scope="module")
def lisp_file():
    # Créez un fichier Lisp de test.
    with open("test.lisp", "w") as f:
        f.write("(+ 1 2)")  # Une simple expression Lisp pour l'addition.
    yield "test.lisp"
    os.remove("test.lisp")

@pytest.fixture(scope="module")
def bc_file(lisp_file):
    # Appeler le programme `glados` pour compiler le fichier Lisp.
    subprocess.run(["./../gladdos", "-c", lisp_file], check=True)
    yield "test.bc"
    os.remove("test.bc")

def test_glados_compile_lisp_file(lisp_file):
    # Appeler le programme `glados` avec l'option de compilation.
    result = subprocess.run(["./../gladdos", "-c", lisp_file], capture_output=True, text=True, check=True)
    # Vérifiez que la compilation a réussi.
    assert "Bytecode écrit avec succès dans le fichier .bc." in result.stdout

def test_glados_execute_bc_file(bc_file):
    # Appeler le programme `glados` pour exécuter le fichier .bc.
    result = subprocess.run(["./../gladdos", bc_file], capture_output=True, text=True, check=True)
    # Pour cet exemple, je suppose qu'une exécution réussie du fichier .bc affiche "3", 
    # mais vous devriez ajuster cette assertion en fonction du comportement attendu.
    assert result.stdout.strip() == "3"
