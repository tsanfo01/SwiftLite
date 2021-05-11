import os
import sys

os.system("./toplevel.native " + sys.argv[1] + " > " + "test.ll")
os.system("llc --relocation-model=pic " + "test.ll > " + "test.s")
os.system("cc -o " + "test.exe " + "test.s")