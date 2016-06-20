from lisp.interpreter import Interpreter
from lisp.LispErrors import *
import readline
import sys
import traceback

import code
import atexit
import os

PROMPT = "> "

def init_history(histfile):
    """Load file with command line history"""
    readline.parse_and_bind("tab: complete")
    if hasattr(readline, "read_history_file"):
        try:
            readline.read_history_file(histfile)
        except IOError:
            pass
        atexit.register(save_history, histfile)

def save_history(histfile):
    """Save command line history to file"""
    readline.set_history_length(1000)
    readline.write_history_file(histfile)

def interpret_file(filename, interpreter, all_results=False):
    """Run interpreter on a file"""
    with open(filename, 'r') as f:
        results = interpreter.evaluate(f.read())
    return "\n".join([str(result) for result in results]) if all_results else str(results[-1]) 

def repl(interpreter=None, error_report=False):
    """Run a read, eval, print loop"""
    interpreter = Interpreter() if interpreter is None else interpreter
    hist_file = os.path.expanduser("~/.mary-history")
    init_history(hist_file)
    while True:
        try:
            next = input(PROMPT)
            if next in {"quit", "exit", "q"}:
                break
            print(interpreter.evaluate(next, False))
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
