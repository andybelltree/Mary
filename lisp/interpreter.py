from .parser import *
from .LispExpression import *
from .Environment import DefaultEnvironment
import sys

DEBUG = True
SHOW_MACROEXPANSION = False

class Interpreter(object):
    """Interprets source code"""
    def __init__(self, debug=False, showmacros=False):
        self.debug = debug
        self.showmacros = showmacros
        self.env = DefaultEnvironment(self)

    def print_debug(self, format_str, *objects):
        objects = [repr(obj) for obj in objects]
        if self.debug:
            try:
                print(format_str.format(*objects))
            except Exception as e:
                print("UNPRINTABLE: {}".format(e))

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
                self.print_debug("INTERPRETING EXPRESSION: {}", expr)
                next_result = expr.evaluate(self.env)
                self.print_debug("RESULT: {}".format(next_result))
                # Convert back to list
                results.append(next_result)
            return results
        else:
            self.print_debug("INTERPRETING EXPRESSION: {}", ast)
            result = ast.evaluate(self.env)
            self.print_debug("RESULT: {}".format(result))
            # Convert back to the form we received it in
            return result
