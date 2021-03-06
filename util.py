"""
Provides utility functions for Mary. These include the repl (Read, Eval, Print Loop)
and a file interpreter.
"""

import readline
import sys
import traceback

import code
import atexit
import os

from lisp.LispErrors import LispError

PROMPT = "> "
KEYWORDS = {"import":"import",
            "file":"import",
            "quit":"quit",
            "exit":"quit",
            "q":"quit",
            "macros":"mode_edit",
            "debug":"mode_edit",
            "environments":"mode_edit",
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

def resolve_keyword(keyword, line, interpreter):
    """Performs commands associated with keywords

    import <library> : interpret contents of a given lisp library
    quit, exit, q : exit the repl
    (macros|environments|debug) (on|off) : change feedback level
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
            print(interpreter.interpret_file(argument + ".lisp"))
        elif KEYWORDS[keyword] == "mode_edit":
            if argument not in {"on", "off"}:
                raise KeywordError(keyword, "Argument must be 'on' or 'off'.")
            if (argument == "on") != interpreter.mode()[keyword]:
                interpreter.toggle(keyword)
        else:
            raise KeywordError(keyword, "No Argument provided")


def repl(interpreter, error_report=False):
    """Run a read, eval, print loop"""
    hist_file = os.path.expanduser("~/.mary-history")
    init_history(hist_file)
    while True:
        try:
            line = input(PROMPT)
            keyword_line = False
            for keyword in KEYWORDS:
                if line.lower().startswith(keyword) and (
                    len(line) == len(keyword) or line[len(keyword)] == " "):
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
