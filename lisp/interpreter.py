from .parser import *
from .LispExpression import *
from .Environment import DefaultEnvironment
import sys

DEBUG = True
SHOW_MACROEXPANSION = False

class Interpreter(object):
    """Interprets source code"""
    def __init__(self, debug=False, showmacros=False, eval_history=False):
        self.debug = debug
        self.showmacros = showmacros
        self.eval_history = eval_history
        self.env = DefaultEnvironment(self)

    def print_debug(self, format_str, *objects):
        objects = [repr(obj) for obj in objects]
        if self.debug:
            try:
                print(format_str.format(*objects))
            except Exception as e:
                print("UNPRINTABLE: {}".format(e))

    def print_eval_history(self, expr):
        if self.eval_history:
            print("\nEvaluation History:")
            for i, e in enumerate(expr.eval_history()):
                print("-" * i + ">{}".format(e))

                
    def print_macroexpansion(self, format_str, *objects):
        objects = [repr(obj) for obj in objects]
        if self.showmacros:
            print(format_str.format(*objects))

    def evaluate(self, src, multiple_expressions=True):
        """Parse the source string then evaluate. If multiple expressions,
        only return the result from the last expression"""
        ast = Parser(src, multiple_expressions).result()
        if multiple_expressions:
            results = []
            for expr in ast:
                next_result = expr.evaluate(self.env)
                self.print_eval_history(next_result)
                # Convert back to list
                results.append(next_result)
            return results
        else:
            result = ast.evaluate(self.env)
            self.print_eval_history(result)
            # Convert back to the form we received it in
            return result
