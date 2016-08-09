"""Keeps track of history of evaluation"""

class EvalHistory(object):
    """A class for storing the evaluation history of an expresion for stack
    tracing and debugging feedback"""
    def __init__(self, lisp_expression):
        self.lisp_exp = lisp_expression
        self.children = []
        self.result = None
        self.eval_env = None

    def print_envs(self):
        """Print the environments created to evaluate this expression"""
        if self.eval_env:
            print("\n" + str(self.eval_env) + "\n")
        if self.result:
            for child in self.children:
                child.eval_hist.print_envs()
            self.result.eval_hist.print_envs()

    def print_eval(self, depth, verbose=False, char='-'):
        """Print all steps taken to evaluate this expression. If verbose, will print out
        all evaluation environments"""
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
        """Adds children (eg. arguments to the expression which need evaluation)"""
        self.children += children

    def add_result(self, result):
        """Adds a resulting expression - the result of this expression's evaluation"""
        self.result = result

    def add_env(self, env):
        """Adds an environment to this expression"""
        self.eval_env = env
