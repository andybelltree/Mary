"""
An interpreter. Parses a string then evaluates it.
Has an environment in which it calls each expression. Subsequent
calls to evaluate new expressions will be called in the same environment.
"""

from os.path import dirname, join
from .parser import Parser
from .Environment import DefaultEnvironment
import sys


class Interpreter(object):
    """Interprets source code"""
    def __init__(self, env=None, env_output=False, showmacros=False, eval_history=False, clear_env=False):
        self.env_output = env_output
        self.showmacros = showmacros
        self.eval_history = eval_history
        self.env = env if env else DefaultEnvironment()
        if not clear_env:
            for lib in env.libs:
                self.interpret_file(join(dirname(__file__), lib))

    def print_env_output(self, expr):
        print("\nEvaluation environments:")
        expr.eval_hist.print_envs()
        
    def print_eval_history(self, expr):
        print("\nEvaluation History:")
        expr.eval_hist.print_eval(1, self.env_output)

    def print_macro_history(self, expr):
        print("\nMacro Expansions:")
        expr.eval_hist.print_macros(1)

    def toggle(self, mode):
        """Turn debug, macros or verbose on or off"""
        if mode == "environments":
            self.env_output = not self.env_output
        elif mode == "macros":
            self.showmacros = not self.showmacros
        elif mode == "debug":
            self.eval_history = not self.eval_history

    def mode(self):
        """Return dictionary with current mode"""
        return {"environments":self.env_output,
                "macros":self.showmacros,
                "debug":self.eval_history}
                
    def interpret_expression(self, expr):
        """Interpret the given expression and return the result"""
        try:
            result = expr.evaluate(self.env, self.eval_history)
            if self.env_output and not self.eval_history:
                self.print_env_output(expr)
            if self.eval_history:
                self.print_eval_history(expr)
            if self.showmacros:
                self.print_macro_history(expr)
        except Exception as e:
            self.print_eval_history(expr)
            raise e
        return result

    def interpret_file(self, filename, all_results=False):
        """Run interpreter on a file"""
        with open(filename, 'r') as f:
            results = self.evaluate(f.read())
        return "\n".join([str(result) for result in results]) if all_results else str(results[-1])
    
    def evaluate(self, src, multiple_expressions=True):
        """Parse the source string then evaluate. If multiple expressions,
        only return the result from the last expression"""
        ast = Parser(src, multiple_expressions).result()
        if multiple_expressions:
            return [self.interpret_expression(expr) for expr in ast]
        else:
            return self.interpret_expression(ast)

