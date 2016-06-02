from .parser import *
from .LispExpression import *
from .Environment import DefaultEnvironment
import sys

DEBUG = True
SHOW_MACROEXPANSION = False

class Interpreter(object):
    """Interprets source code"""
    def __init__(self, env=None, debug=False, showmacros=False, eval_history=False):
        self.debug = debug
        self.showmacros = showmacros
        self.eval_history = eval_history
        self.env = env if env else DefaultEnvironment()

    def print_eval_history(self, expr):
        print("\nEvaluation History:")
        expr.print_eval(1, self.debug)

    def print_macro_history(self, expr):
        print("\nMacro Expansions:")
        expr.print_macros(1)
                
    def interpret_expression(self, expr):
        try:
            result = expr.evaluate(self.env)
            if self.eval_history or self.debug:
                self.print_eval_history(expr)
            if self.showmacros:
                self.print_macro_history(expr)
        except Exception as e:
            self.print_eval_history(expr)
            raise e
        return result
            
    def evaluate(self, src, multiple_expressions=True):
        """Parse the source string then evaluate. If multiple expressions,
        only return the result from the last expression"""
        ast = Parser(src, multiple_expressions).result()
        if multiple_expressions:
            return [self.interpret_expression(expr) for expr in ast]
        else:
            return self.interpret_expression(ast)

