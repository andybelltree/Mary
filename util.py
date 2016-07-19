from lisp.interpreter import Interpreter
from lisp.LispErrors import *
import readline
import sys
import traceback

import code
import atexit
import os

PROMPT = "> "
KEYWORDS = {"import":"import",
            "file":"import",
            "quit":"quit",
            "exit":"quit",
            "q":"quit",
            "macros":"mode_edit",
            "debug":"mode_edit",
            "verbose":"mode_edit",
            "mode":"mode"}

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

def resolve_keyword(keyword, line, interpreter):
    """Performs commands associated with keywords

    import <library> : interpret contents of a given lisp library
    quit, exit, q : exit the repl
    (macros|verbose|debug) (on|off) : change feedback level
    mode : display current mode
    
    """
    if KEYWORDS[keyword] == "quit":
        exit(0)
    elif KEYWORDS[keyword] == "mode":
        for key, val in interpreter.mode().items():
            print("{} : {}".format(key, "On" if val else "Off"))
    elif len(line) > len(keyword) and line[len(keyword)] == " ":
        argument = line.split()[1]
        if KEYWORDS[keyword] == "import":
            print(interpret_file(argument + ".lisp", interpreter))
        elif KEYWORDS[keyword] == "mode_edit":
            if argument not in {"on", "off"}:
                raise KeywordError(keyword, "Argument must be 'on' or 'off'.")
            if (argument == "on") != interpreter.mode()[keyword]:
                interpreter.toggle(keyword)                
        else:
            raise KeywordError(keyword, "No Argument provided")


def repl(interpreter=None, error_report=False):
    """Run a read, eval, print loop"""
    interpreter = Interpreter() if interpreter is None else interpreter
    hist_file = os.path.expanduser("~/.mary-history")
    init_history(hist_file)
    while True:
        try:
            line = input(PROMPT)
            keyword_line = False
            for keyword in KEYWORDS:
                if line.lower().startswith(keyword):
                    keyword_line = True
                    resolve_keyword(keyword, line, interpreter)
            if not keyword_line:
                print(interpreter.evaluate(line, False))
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
