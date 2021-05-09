import os
import subprocess

# Path to the LLVM interpreter
LLI = "lli"
#LLI="/usr/local/opt/llvm/bin/lli"

# Path to the LLVM compiler
LLC = "llc"

# Path to the C compiler
CC = "cc"

compile_succ_tests = [
    "test-print",
    "test-var",
    "test-assign",
    "test-let",
    "test-if1",
    "test-if2",
    "test-if3",
    "test-if4",
    "test-while1",
    "test-char",
    "test-scope1",
    "test-scope2",
    "test-scope3",
    "test-scope4",
    "test-scope5",
    "test-scope6",
    "test-arrat1",
    "test-arrat2",
    "test-arrat3",
    "test-arrassign1",
    "test-arrassign2",
    "test-emptyarr",
    "test-func1",
    "test-func2",
    "test-func3",
    "test-func4",
    "test-func5",
    "test-func6",
    "test-func7",
    "test-for1",
    "test-for2",
    "test-for3",
    "test-for4",
    "test-return",
    "test-opt1",
    "test-opt2",
    "test-opt3",
    "test-opt4",
    "test-opt5",
    "test-opt6",
    "test-opt7",
    "test-concat1",
    "test-concat2",
    "test-concat3",
    "test-concat4",
    "test-concat5",
    "test-range1",
    "test-range2",
    "test-enum1",
    "test-enum2",
    "test-enum3",
    "test-class1",
    "test-class2",
    "test-class3",
    "test-class4",
    "test-class5",
    "test-class6",
    "test-class7",
]

compile_fail_tests = [
    "fail-var1",
    "fail-var2",
    "fail-assign1",
    "fail-assign2",
    "fail-if1",
    "fail-if2",
    "fail-if3",
    "fail-while1",
    "fail-while2",
    "fail-scope1",
    "fail-scope2",
    "fail-arrat1",
    "fail-arrat2",
    "fail-arrassign1",
    "fail-arrassign2",
    "fail-arrassign3",
    "fail-func1",
    "fail-func2",
    "fail-func3",
    "fail-func4",
    "fail-func5",
    "fail-for1",
    "fail-for2",
    "fail-for3",
    "fail-opt1",
    "fail-opt2",
    "fail-opt3",
    "fail-opt4",
    "fail-opt5",
    "fail-enum1",
    "fail-enum2",
    "fail-enum3",
    "fail-enum4",
    "fail-enum5",
    "fail-class1",
    "fail-class2",
    "fail-class3",
    "fail-class4",
    "fail-class5",
    "fail-class6",
    "fail-class7",
    "fail-class8",
]

command = "./toplevel.native"

def testFail(tests):
    print("\n")
    for test in tests:
        process = subprocess.run([command, "tests/" + test + ".swl"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        output = process.stdout
        path = "tests/" + test + ".err"
        expected_output = open(path,"r").read()
        if output == expected_output: 
            print("Running tests: " + test + "...OK")
        else:
            print("Running tests: " + test + "...FAILED")

def testCompile (tests):
    print("\n")
    for test in tests:
        os.system(command + " tests/" + test + ".swl > " + test + ".ll")
        os.system(LLC + " --relocation-model=pic " + test + ".ll > " + test + ".s")
        os.system(CC +  " -o " + test + ".exe " + test + ".s")
        process = subprocess.run(["./" + test + ".exe"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        output = process.stdout
        path = "tests/" + test + ".out"
        expected_output = open(path,"r").read()
        if output == expected_output: 
            print("Running tests: " + test + "...OK")
            os.system("rm " + test + ".*")
        else:
            print("Running tests: " + test + "...FAILED\n" + "Expected: " + expected_output + "Got: " + output)


testCompile(compile_succ_tests)
testFail(compile_fail_tests)