import os
import subprocess
from typing import Optional, List

from colorama import Fore


def read_test(index: int) -> Optional[str]:
    path = f"./lispfile/test{index}.lisp"
    if not os.path.exists(path):
        path = f"./test/lispfile/test{index}.lisp"
    if not os.path.exists(path):
        return None
    f = open(path, "r")
    content = f.read()
    f.close()
    return content


def write_test(content: str) -> None:
    f = open("./test.lisp", "w")
    f.write(content)
    f.close()


def parse_test(code: str) -> dict:
    lines = code.split("\n")
    result = {"code": "", "result": ""}
    for line in lines:
        if line.startswith("->"):
            result["result"] += (line
                                 .replace("->", "")
                                 .replace(" ", "")
                                 .replace("\n", ""))
        else:
            result["code"] += line + "\n"
    return result


def run_test(code: dict) -> List[str]:
    write_test(code["code"])
    result = subprocess.run(['../gladdos', '-c', './test.lisp'],
                            capture_output=True)
    compilation = result.stdout.decode("utf-8").replace("\n", "")
    if not os.path.exists(".bc"):
        return ["COMPILATION ERROR", compilation]
    result = subprocess.run(['../gladdos', '.bc'],
                            capture_output=True)
    stdout = result.stdout
    line = stdout.decode("utf-8").replace("\n", "")
    os.remove("./test.lisp")
    os.remove(".bc")
    if line == code["result"]:
        return ["OK", line]
    return ["KO", line]


def print_result(index: int, result: list):
    prefix = result[0]
    print(f"#{index}\t->", end="")
    if prefix == "OK":
        print(Fore.GREEN, prefix, Fore.RESET)
    else:
        print(Fore.RED, prefix, Fore.RESET, "; DETAILS: ", result[1])


if __name__ == "__main__":
    i = 1
    while True:
        code = read_test(i)
        if code is None:
            break
        code = parse_test(code)
        code = run_test(code)
        print_result(i, code)
        i += 1
