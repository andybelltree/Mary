#!/usr/bin/env python3

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
    print("** Results of {} **".format(testname))
    try:
        results = interpret_file(testfile, interpreter, True).splitlines()
    except RuntimeError:
        print("Maximum recursion depth exceeded in {}\n".format(testname))
    except LispError as e:
        print("ERROR: \"" + e.msg + "\" in " + testname + "\n")
    else:
        expected_results = extract_comments(testfile).splitlines()
        d = list(diff.compare(expected_results, results))
        found_diff = False
        bad_rows = []
        for row in d:
            if not(row.strip() == "" or row.startswith(" ")):
                print("#", end="")
                bad_rows.append(row)
            else:
                print(".", end="")
        print()
        if len(bad_rows) == 0:
            print("Output matches expected")
        else:
            for bad_row in bad_rows:
                print(bad_row)
        print()
    
def main():

    testdir = join(dirname(__file__), "tests")
    mintestdir = join(dirname(__file__), "mintests")
    testfiles = [(join(testdir, filename), filename[:-5]) for filename in listdir(testdir) if filename.endswith(".lisp")]
    mintestfiles = [(join(mintestdir, filename), filename[:-5]) for filename in listdir(mintestdir) if filename.endswith(".lisp")]

    for env, title, files in [(DefaultEnvironment(), "With defun:", testfiles),
                                    (MacroEnvironment(), "Without defun:", testfiles),
                                    (MinimumEnvironment(), "Minimal Lisp:", mintestfiles)]:
        interpreter = Interpreter(env)
        libs = env.libs
        print("="*5 + title + "="*5 + "\n")
        for testfile, testname in files:
            for lib in libs:
                interpret_file(join(dirname(__file__), lib), interpreter)
            runtest(interpreter, testfile, testname)

if __name__ == "__main__":
    main()

