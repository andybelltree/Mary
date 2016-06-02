#!/usr/bin/env python

from os.path import dirname, join
from os import listdir
from util import interpret_file
from lisp.interpreter import Interpreter
from lisp.LispErrors import *
from difflib import Differ
from lisp.Environment import *

def extract_comments(filename):
    comments = []
    with open(filename, 'r') as f:
        for line in f:
            line = line.split(";", 1)
            if len(line) > 1:
                line = line[1]
                comments.append(line.strip("; \n"))
    return "\n".join(comments)

def runtest(interpreter, testfile, testname):
    diff = Differ()
    try:
        results = interpret_file(testfile, interpreter, True).splitlines()
    except RuntimeError:
        print("Maximum recursion depth exceeded in {}\n".format(testname))
    except LispError as e:
        print("ERROR: \"" + e.msg + "\" in " + testname + "\n")
    else:
        expected_results = extract_comments(testfile).splitlines()
        d = list(diff.compare(expected_results, results))
        print("** Results of {} **".format(testname))
        #i = 0
        #row = 0
        found_diff = False
        for row in d:
            if not(row.strip() == "" or row.startswith(" ")):
                found_diff = True
                print(row)
        if not found_diff:
            print("Output matches expected")
        print()
    
def main():

    testdir = join(dirname(__file__), "tests")
    mintestdir = join(dirname(__file__), "mintests")
    testfiles = [(join(testdir, filename), filename[:-5]) for filename in listdir(testdir) if filename.endswith(".lisp")]
    mintestfiles = [(join(mintestdir, filename), filename[:-5]) for filename in listdir(mintestdir) if filename.endswith(".lisp")]

    for interpreter, title, lib, files in [(Interpreter(DefaultEnvironment()), "With defun:", "stdlib.lisp", testfiles),
                                    (Interpreter(MacroEnvironment()), "Without defun:", "macrostdlib.lisp", testfiles),
                                    (Interpreter(MinimumEnvironment()), "Minimal Lisp:", "minstdlib.lisp", mintestfiles)]:
        print(title)
        for testfile, testname in files:
            interpret_file(join(dirname(__file__), lib), interpreter)
            runtest(interpreter, testfile, testname)

if __name__ == "__main__":
    main()

