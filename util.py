from lisp.interpreter import Interpreter
from lisp.parser import unparse
from lisp.LispErrors import *
import readline
import sys
import traceback

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
    while True:
        try:
            print(">", end = "")
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
