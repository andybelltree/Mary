"""
An environment in which to evaluate a LispExpression. Each environment has a set of symbols
which correspond to a LispExpression.

Contains default environments which define sets of axioms. To create a new default environment
with different axioms, inherit from the BaseEnvironment class and implement the _define_defaults
method. This should be used to define any axioms one wants. Either make calls to existing definitions or
write your own.
"""
from .LispErrors import *
from .LispExpression import *
from functools import reduce
import sys

LAMBDA = "lambda"
DEFMACRO = "defmacro"
QUOTE = "quote"
IF = "if"
CAR = "car"
CDR = "cdr"
CONS = "cons"
IS_ATOM = "atom?"

SUBTRACT = "-"
LESSTHAN = "<"

QUASIQUOTE="quasiquote"
UNQUOTE="unquote"
SPLICE="splice"

PRINT = "printsym"
INPUT_CHAR = "inputchar"

GENSYM = "gensym"
GENSYM_ESCAPE = "#"

DEFUN = "defun"

PYCALL = "pycall"
        
class Environment(object):
    """An environment of definitions in which to interpret a Lisp Expression"""
    def __init__(self, parent_environment):
        """Initialises the environment with its parent and an empty set of definitions"""
        self.parent_environment = parent_environment
        self.definitions = {}
        self.libs = []

    def retrieve_definition(self, label):
        """Get definition of label in environment if it exists"""
        if not type(label) == SymbolExpression:
            raise TypeError("definition retrieval", label, "Symbol Expression")
        if label.value in self.definitions:
            return self.definitions[label.value]
        elif self.parent_environment:
            return self.parent_environment.retrieve_definition(label)
        else:
            # If we have no definition then raise an error
            raise UnknownLabelError(label.value)

    def define(self, label, value):
        """Define a new function or value"""
        if not type(label) == SymbolExpression:
            raise TypeError("definition", label, "Symbol Expression")
        self.definitions[label.value] = value
        return label

    def create_child(self):
        return Environment(self)

    def __repr__(self):
        """Useful for debugging."""
        form_str = "{:^100}"
        if issubclass(self.__class__, BaseEnvironment):
            return form_str.format("{DEFAULT ENVIRONMENT}")
        return (
            "" if self.parent_environment is None else ("{}\n"+form_str+"\n"+form_str).format(
            self.parent_environment, "||", str({
                k:repr(v) for k,v in self.definitions.items()}))) 
    

class BaseEnvironment(Environment):
    """A default environment, with axiomatic function definitions"""
    def __init__(self):
        """Creates the default environment"""
        super(BaseEnvironment, self).__init__(None)
        self._define_defaults()

    def _define_defaults(self):
        raise NotImplementedError

    def _define_defmacro(self):
        """Defines the defmacro function"""
        def defmacro(args, env):
            return env.define(args[0], MacroExpression(args[1:], args[0]))
        self.define(SymbolExpression(DEFMACRO), LispFunction(DEFMACRO, defmacro, 3))

    def _define_lambda(self):
        """Defines the lambda function"""
        self.define(SymbolExpression(LAMBDA), LispFunction(
            LAMBDA,
            lambda args, env : LambdaExpression(args, env)))

    def _define_defun(self):
        """Defines function definition"""
        def defun(args, env):
            return env.define(args[0], LambdaExpression(args[1:], env))
        self.define(SymbolExpression(DEFUN), LispFunction(DEFUN, defun, 3))

    def _define_quote(self):
        """Defines the quote function"""
        def quote(args, env):
            return args[0]
        self.define(SymbolExpression(QUOTE), LispFunction(QUOTE, quote, 1))


    def _define_quasiquote(self):
        """Defines backquote"""
        def quasiquote(args, env):
            """Backquote function for quasiquote read macro"""
            def quasiquote_expand(args, backquotes):
                """Called recursively by backquote function
                Returns True or false depending on whether results should be 
                spliced into upper layer"""
                # if there as many commas as backquotes encountered, evaluate the expression
                if backquotes == 0:
                    return args.evaluate(env)
                # if you have an atom or an empty list, just return
                if args.atom() or args.is_empty():
                    return make_list(args)
                elif tagged(args, UNQUOTE):                    
                    # If you see a comma, begin evaluation again, then put it in a list
                    result = quasiquote_expand(tagged_data(args), backquotes-1)
                    # Special case: The layer below is a splice, which means this comma
                    # needs to be applied to each item in the resulting list.
                    if not tagged_data(args).atom() and tagged(tagged_data(args), SPLICE):
                        return ListExpression([tag(UNQUOTE, exp) for exp in result.value])
                    else:
                        return make_list(result if backquotes == 1 else tag(UNQUOTE, result.car()))
                elif tagged(args, SPLICE):
                    result = quasiquote_expand(tagged_data(args), backquotes-1)
                    return result if backquotes == 1 else make_list(tag(SPLICE, result.car()))
                elif tagged(args, QUASIQUOTE):
                    # Nested back quotes? We're going deeper. Count it up
                    return make_list(tag(QUASIQUOTE, quasiquote_expand(
                        tagged_data(args), backquotes+1).car()))
                else:
                    return make_list(
                        ListExpression(
                            reduce(lambda x,y: x+quasiquote_expand(y, backquotes).value,
                                   args.value,
                                   [])))

            def tagged(args, tag):
                """True iff data has given tag"""
                return args.value[0].value == tag

            def tagged_data(args):
                """Returns the tagged data"""
                return args.cdr().car()
                
            def tag(tag, args):
                """Adds a tag to the argument"""
                return ListExpression([SymbolExpression(tag), args])

            def make_list(args):
                """Turns argument into a list"""
                return ListExpression([args])

            return quasiquote_expand(args[0], 1).car()


        self.define(SymbolExpression(QUASIQUOTE), LispFunction(QUASIQUOTE, quasiquote, 1))

        
    def _define_if(self):
        """Defines the if 'function'"""
        def if_ex(args, env):
            if args[0].evaluate(env).is_nill():
                return args[2].evaluate(env) if len(args) > 2 else Nils.nil
            else:
                return args[1].evaluate(env)
        self.define(SymbolExpression(IF), LispFunction(IF, if_ex, 2))

    def _define_atom(self):
        """Defines the atom function. True if argument is an atom, else ()"""
        def atom(args, env):
            arg = args[0].evaluate(env)
            return arg if arg.atom() else Nils.nil
        self.define(
            SymbolExpression(IS_ATOM), LispFunction(
                IS_ATOM, atom, 1))

    def _define_car(self):
        """Defines the car function"""
        def car(args, env):
            exp = args[0].evaluate(env)
            if not (issubclass(type(exp), AtomExpression) or type(exp) == ListExpression):
                raise TypeError(CAR, exp, "List or Symbol")
            return exp.car()
        self.define(SymbolExpression(CAR), LispFunction(CAR, car, 1))

    def _define_cdr(self):
        """Defines the cdr function"""
        def cdr(args, env):
            exp = args[0].evaluate(env)
            if not (issubclass(type(exp), AtomExpression) or type(exp) == ListExpression):
                raise TypeError(CDR, exp, "List or Symbol")
            return exp.cdr()
        self.define(SymbolExpression(CDR), LispFunction(CDR, cdr, 1))

    def _define_cons(self):
        """Defines the cons function"""
        def cons(args, env):
            expr1 = args[0].evaluate(env)
            expr2 = args[1].evaluate(env)
            if not (type(expr2) == ListExpression or
                    (issubclass(type(expr2), AtomExpression) and
                                issubclass(type(expr1), AtomExpression))):
                raise TypeError(CONS, str(expr1) + " and " + str(expr2), "Lists or Symbols")
            return expr2.cons(expr1)
                
        self.define(SymbolExpression(CONS), LispFunction(CONS, cons, 2))

    def _define_subtract(self):
        """Defines the subtract function"""        
        def subtract(args, env):
            expr1 = args[0].evaluate(env)
            expr2 = args[1].evaluate(env)
            if not type(expr1) == type(expr2) == NumberExpression:
                raise TypeError(SUBTRACT, "{} and {}".format(expr1, expr2), "Numbers")
            else:
                return expr1.subtract(expr2)
        self.define(SymbolExpression(SUBTRACT), LispFunction(SUBTRACT, subtract, 2))

    def _define_lessthan(self):
        """Defines the less than function"""        
        def lessthan(args, env):
            expr1 = args[0].evaluate(env)
            expr2 = args[1].evaluate(env)
            try:
                return expr1.lessthan(expr2)
            except AttributeError:
                raise TypeError(LESSTHAN, "{} and {}".format(expr1, expr2), "Numbers or symbols")

        self.define(SymbolExpression(LESSTHAN), LispFunction(LESSTHAN, lessthan, 2))

    def _define_print_sym(self):
        """Defines print function"""
        def print_sym(args, env):
            print_val = args[0].evaluate(env)
            if print_val.is_empty():
                print("nil", end="")
            else:
                if not issubclass(type(print_val), AtomExpression):
                    raise TypeError(PRINT, type(print_val), "Atom")
                print(print_val.formatted(), end="")
            sys.stdout.flush()
            return Nils.null # Return an empty string
        self.define(SymbolExpression(PRINT), LispFunction(PRINT, print_sym, 1))

    def _define_input_char(self):
        """Defines input char function"""
        # getch function for getting raw input. Platform specific
        try:
            import tty, termios
        except ImportError:
            try:
                import msvcrt
            except ImportError:
                # input will not work. Just read characters from input. Will be buggy.
                getch = lambda : sys.stdin.read(1)
            else:
                # Use windows method
                getch = msvcrt.getch
        else:
            def getch():
                """Reads a single key from input. Doesn't wait for enter to be pressed."""
                fd = sys.stdin.fileno()
                old_settings = termios.tcgetattr(fd)
                try:
                    tty.setraw(fd)
                    ch = sys.stdin.read(1)
                finally:
                    termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
                return ch
            
        def input_char(args, env):
            ch = getch()
            # Replace any carriage returns with newlines
            if ord(ch) == 13:
                ch = "\\n"
                print("\n", end="")
            else:
                print(ch, end="")
            if ch == " ":
                ch = "\\s"
            sys.stdout.flush()
            return LispExpression.create_atom(ch)

        self.define(SymbolExpression(INPUT_CHAR), LispFunction(INPUT_CHAR, input_char))

    def _define_gensym(self):
        """Defines the gensym function to generate a symbol that doesn't yet exist"""
        counter = 0
        def gensym(args, env):
            nonlocal counter
            counter += 1
            return SymbolExpression(GENSYM_ESCAPE + str(counter))
        self.define(SymbolExpression(GENSYM), LispFunction(GENSYM, gensym))

class MacroEnvironment(BaseEnvironment):
    def _define_defaults(self):
        """Defines default functions not including defun"""
        self._define_lambda()
        self._define_defmacro()

        self._define_quote()
        self._define_if()
        self._define_car()
        self._define_cdr()
        self._define_cons()
        self._define_atom()
        
        self._define_quasiquote()
        
        self._define_subtract()
        self._define_lessthan()
        
        self._define_print_sym()
        self._define_input_char()

        self._define_gensym()

        self.libs = ["macrostdlib.lisp", "stdlib.lisp"]


class DefaultEnvironment(MacroEnvironment):
    def _define_defaults(self):
        """Defines all default functions"""
        super(DefaultEnvironment, self)._define_defaults()
        self._define_defun()
        self.libs = ["fnstdlib.lisp", "stdlib.lisp"]

class MinimumEnvironment(BaseEnvironment):
    def _define_defaults(self):
        """Defines a minimal set of default functions"""
        self._define_lambda()
        self._define_defun()
        self._define_quote()
        self._define_if()

        self.libs = ["minstdlib.lisp"]

class PythonEnvironment(DefaultEnvironment):
    def _define_pythoncall(self):
        """Defines the python call function"""
        def pycall(args, env):
            if len(args) < 1:
                raise WrongNumParamsError(PYCALL, 1, len(args))
            pyfunc = args[0].evaluate(env)
            if not pyfunc.atom():
                raise BadInputError("python function", pyfunc)
            python_exp = "{}({})".format(
                str(pyfunc),
                ", ".join([str(arg.evaluate(env).value) for arg in args[1:]]))
            try:
                result = eval(python_exp)
            except Exception as e:
                raise BadInputError("Python expression", python_exp)
            if type(result) == list:
                return ListExpression([LispExpression.create_atom(r) for r in result])
            else:
                return LispExpression.create_atom(result)
        self.define(SymbolExpression(PYCALL), LispFunction(PYCALL, pycall))
    
    def _define_defaults(self):
        """Allows for calling of arbitrary python functions"""
        super(PythonEnvironment, self)._define_defaults()
        self._define_pythoncall()

