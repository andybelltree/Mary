#!/usr/bin/env python3

from time import perf_counter
from random import shuffle
import matplotlib.pyplot as plt
import sys
import os

PACKAGE_PARENT = '..'
SCRIPT_DIR = os.path.dirname(os.path.realpath(os.path.join(os.getcwd(), os.path.expanduser(__file__))))
sys.path.append(os.path.normpath(os.path.join(SCRIPT_DIR, PACKAGE_PARENT)))

from lisp.LispExpression import *
from lisp.Environment import DefaultEnvironment
from lisp.interpreter import Interpreter

file_name = 'times'

def quicksort(a_list):
    quicksort_helper(a_list,0, len(a_list)-1)
    return a_list

def quicksort_helper(a_list,first,last):
    if first < last:
        pivot = partition(a_list,first,last)
        quicksort_helper(a_list,first,pivot-1)
        quicksort_helper(a_list,pivot+1,last)

def partition(a_list,first,last):
    pivot = a_list[first]

    leftmark = first+1
    rightmark = last

    done = False
    while not done:
        while leftmark <= rightmark and a_list[leftmark] <= pivot:
            leftmark = leftmark + 1
        
        while a_list[rightmark] >= pivot and rightmark >= leftmark:
            rightmark = rightmark -1
        
        if rightmark < leftmark:
            done = True
            
        else:
            a_list[leftmark], a_list[rightmark] = a_list[rightmark],a_list[leftmark]
    
    a_list[first], a_list[rightmark] = a_list[rightmark], a_list[first]
    return rightmark

def main():
    sys.setrecursionlimit(10000)
    ns = range(10,520,50)
    num_trials = 50

    mary_times = []
    python_times = []

    mary_interpreter = Interpreter(DefaultEnvironment())
    
    for n in ns:
        print("Timing Quicksort for {}".format(n))
        mary_times_n = []
        python_times_n = []
        for _ in range(num_trials):
            to_sort = list(range(n))
            shuffle(to_sort)
            mary_exp = ListExpression([SymbolExpression("quicksort"),ListExpression([SymbolExpression("quote"), ListExpression([LispExpression.create_atom(m) for m in to_sort])])])
            python_start = perf_counter()
            quicksort(to_sort)
            python_end = perf_counter()
            mary_interpreter.interpret_expression(mary_exp)
            mary_end = perf_counter()
            python_times_n.append(python_end - python_start)
            mary_times_n.append(mary_end - python_end)
        mary_times.append(sum(mary_times_n)/len(mary_times_n))
        python_times.append(sum(python_times_n)/len(mary_times_n))
        print("Took {:.4f}s for Mary and {:.4f}s for Python".format(mary_times[-1], python_times[-1]))
    print("Ns", ",".join([str(n) for n in ns]))
    print("Quicksort Times for Mary: ", ",".join([str(n) for n in mary_times]))
    print("Times for Python: ", ",".join([str(n) for n in python_times]))
    with open(file_name,'w') as f:
        f.write('# Comparison of quicksort timings between Mary and Python\n')
        f.write('# Num Items Sorted, Time in seconds for Mary, Time in seconds for Python\n')
        for i in range(len(ns)):
            f.write("{},{},{}\n".format(ns[i],mary_times[i],python_times[i]))
    print("Wrote to '{}'".format(file_name))
    plt.plot(ns, mary_times, "bo", python_times, "ro")

if __name__ == "__main__":
    main()
