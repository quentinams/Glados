import pexpect
import sys

# Définition des couleurs pour la sortie du terminal
class TerminalColors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def test_lisp_interpreter(test_number, input_code, expected_output):
    interpreter_path = "./gladdos"

    print(TerminalColors.HEADER + f"Running Test #{test_number}: " + TerminalColors.ENDC)
    print(TerminalColors.OKBLUE + f"Input: {input_code}" + TerminalColors.ENDC)
    print(TerminalColors.OKBLUE + f"Expected Output: {expected_output}" + TerminalColors.ENDC)

    child = pexpect.spawn(interpreter_path)
    child.expect("Enter your lisp expression \(or type 'quit' to exit\):")
    child.sendline(input_code)
    child.expect("Enter your lisp expression \(or type 'quit' to exit\):")
    
    output_lines = child.before.splitlines()
    result = output_lines[-1].strip().decode("utf-8")

    if result == expected_output:
        print(TerminalColors.OKGREEN + "Test passed!" + TerminalColors.ENDC)
        passed = True
    else:
        print(TerminalColors.FAIL + "Test failed!" + TerminalColors.ENDC)
        print("Expected:", expected_output, file=sys.stderr)
        print("Got:", result, file=sys.stderr)
        passed = False
    
    child.sendline("quit")
    child.close()
    print("\n" + TerminalColors.UNDERLINE + "Test End" + TerminalColors.ENDC + "\n")
    
    return passed

# Test data
tests = [
    ("(+ 1)", "1"),
    ("(- 1)", "-1"),
    ("(* 1)", "1"),
    ("(/ 1)", "1"),
    ("(+ 2 3)", "5"),
    ("(- 5 2)", "3"),
    ("(/ 10 2)", "5"),
    ("(div 10 3)", "3"),
    ("(/ 10 4)", "2.5"),
    ("(define zero (mod 10 2))", "0"),
    ("(+ zero 1)", "1"),
    ("(define x (+ 1 1 1 1 1 1 1 1 1 1 zero))", "10"),
    
    # Plus de tests ici si besoin
]

# Compteurs de tests
total_tests = len(tests)
tests_passed = 0

# Exécution des tests
for i, (test_code, expected_output) in enumerate(tests):
    if test_lisp_interpreter(i + 1, test_code, expected_output):
        tests_passed += 1

# Résumé
print("\n" + TerminalColors.BOLD + "Test Summary:" + TerminalColors.ENDC)
print(TerminalColors.OKGREEN + f"Tests Passed: {tests_passed}/{total_tests}" + TerminalColors.ENDC)
print(TerminalColors.FAIL + f"Tests Failed: {total_tests - tests_passed}/{total_tests}" + TerminalColors.ENDC)
