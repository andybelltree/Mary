from lisp.interpreter import Interpreter
from lisp.parser import unparse
from lisp.LispErrors import *
import readline
import sys
import traceback

import code
import atexit
import os

def init_history(histfile):
    readline.parse_and_bind("tab: complete")
    if hasattr(readline, "read_history_file"):
        try:
            readline.read_history_file(histfile)
        except IOError:
            pass
        atexit.register(save_history, histfile)

def save_history(histfile):
    readline.set_history_length(1000)
    readline.write_history_file(histfile)

def interpret_file(filename, interpreter, all_results=False):
    with open(filename, 'r') as f:
        source = f.read()
    results = interpreter.evaluate(source)
    if all_results:
        return "\n".join([unparse(result) for result in results])
    else:
        return unparse(results[-1]) 

def repl(interpreter=None, error_report=False):
    """From kvelle"""
    interpreter = Interpreter() if interpreter is None else interpreter
    hist_file = os.path.expanduser("~/.mary-history")
    init_history(hist_file)
    while True:
        try:
            sys.stdout.write(">")
            print(interpreter.evaluate(input(), False))
        except (EOFError, KeyboardInterrupt):
            return
        except LispError as e:
            print("! {}".format(e.msg))
            if error_report:
                traceback.print_tb(sys.exc_info()[2], 10)
        except SyntaxError as e:
            print("! {}".format(e))
            if error_report:
                traceback.print_tb(sys.exc_info()[2], 10)
        except RuntimeError as e:
            print("! {}".format(e))
            if error_report:
                traceback.print_tb(sys.exc_info()[2], 10)
        except Exception as e:
            print("! {}".format(e))
            if error_report:
                traceback.print_tb(sys.exc_info()[2], 10)
