#!/usr/bin/env python

from os.path import dirname, join
from os import listdir
from util import interpret_file
from lisp.interpreter import Interpreter
from lisp.LispErrors import *
from difflib import Differ

def extract_comments(filename):
    comments = []
    with open(filename, 'r') as f:
        for line in f:
            line = line.split(";", 1)
            if len(line) > 1:
                line = line[1]
                comments.append(line.strip("; \n"))
    return "\n".join(comments)

testdir = join(dirname(__file__), "tests")
testfiles = [(join(testdir, filename), filename[:-5]) for filename in listdir(testdir) if filename.endswith(".lisp")]
diff = Differ()

for testfile, testname in testfiles:
    interpr = Interpreter()
    interpret_file(join(dirname(__file__), "stdlib.lisp"), interpr)
    try:
        results = interpret_file(testfile, interpr, True).splitlines()
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

        # while i < len(d):
        #     if not(d[i].startswith(" ")):
        #         found_diff = True
        #         if (d[i].startswith("-")):
        #             got = d[i][2:]
        #             i += 1
        #             expected = d[i][2:]
        #             chardiff = None
        #             if len(d) > i + 1 and d[i+1].startswith("?"):
        #                 i += 1
        #             print("-- Expected {} but got {} on row {}".format(expected, got, row))
        #         else:
        #             print("--Bad output: {}".format(d[i]))
        #     i += 1
        #     row += 1
        if not found_diff:
            print("Output matches expected")
        print()
