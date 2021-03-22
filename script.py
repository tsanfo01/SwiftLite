import os
import subprocess
import tempfile

succ_tests = [
    "test1",
    "test2",
    "test3",
    "test4",
    "test5",
]

fail_tests = [
    "test6",
    "test7",
    "test8",
    "test9",
    "test10",
]

def test (tests, command, ext):
    print("\n")
    for test in tests:
        process = subprocess.run([command, "tests/" + test + ".swl"], stdout=subprocess.PIPE, text=True, stderr=subprocess.STDOUT)
        output = process.stdout
        path = "tests/" + test + ext
        expected_output = open(path,"r").read()
        if output == expected_output: 
            print("Running tests: " + test + "...OK")
        else:
            print("Running tests: " + test + "...FAILED")

  
        
os.system("ocamlbuild -use-ocamlfind toplevel.native")

test(succ_tests, "./toplevel.native", ".out")
test(fail_tests, "./toplevel.native", ".err")