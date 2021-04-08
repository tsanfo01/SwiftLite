import os
import subprocess

# Path to the LLVM interpreter
LLI = "lli"
#LLI="/usr/local/opt/llvm/bin/lli"

# Path to the LLVM compiler
LLC = "llc"

# Path to the C compiler
CC = "cc"

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

compile_tests = [
    "testprint",
]

def test (tests, command, ext):
    print("\n")
    for test in tests:
        os.system(command + " tests/" + test + ".swl > " + test + ".ll")
        os.system(LLC + " --relocation-model=pic " + test + ".ll > " + test + ".s")
        os.system(CC +  " -o " + test + ".exe " + test + ".s")
        process = subprocess.run(["./" + test + ".exe"], stdout=subprocess.PIPE, text=True)
        output = process.stdout
        path = "tests/" + test + ext
        expected_output = open(path,"r").read()
        if output == expected_output: 
            print("Running tests: " + test + "...OK")
        else:
            print("Running tests: " + test + "...FAILED")
        os.system("rm " + test + ".*")


test(compile_tests, "./toplevel.native", ".out")