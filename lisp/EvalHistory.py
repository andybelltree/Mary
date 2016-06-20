"""Keeps track of history of evaluation"""

class EvalHistory(object):
    # Following are for storing debug information
    def __init__(self, lisp_expression):
        self.lisp_exp = lisp_expression
        self.children = []
        self.result = None
        self.eval_env = None

    def print_eval(self, depth, verbose=False, char='-'):
        """Print all steps taken to evaluate this expression"""
        print((char * 2 + "|") * (depth-1) + char + ">" + str(self.lisp_exp))
        if verbose and self.eval_env:
            print("\n" + str(self.eval_env) + "\n")
        if self.result:
            for child in self.children:
                child.eval_hist.print_eval(depth + 1, verbose, "-")
            self.result.eval_hist.print_eval(depth, verbose, "=")
            
    def print_macros(self, depth):
        """Print all macro expansions done when evaluating this expression"""
        is_macro = len(self.children) > 0 and self.children[
            0].eval_hist.result and self.children[0].eval_hist.result.is_macro()
        if is_macro:
            print("|" * depth)
            print(">" * depth + str(self.lisp_exp))
            print("-" * depth + str(self.children[0].eval_hist.result))
        for child in self.children:
            child.eval_hist.print_macros(depth + 1)
        if is_macro:
            print("=" * depth + (str(self.result) if self.result else "()"))
        if self.result:
            self.result.eval_hist.print_macros(depth)

    def add_children(self, children):
        self.children += children

    def add_result(self, result):
        self.result = result

    def add_env(self, env):
        self.eval_env = env
