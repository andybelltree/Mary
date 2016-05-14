from .LispErrors import *
from .LispExpression import *
from functools import reduce

LAMBDA = "lambda"
DEFMACRO = "defmacro"
QUOTE = "quote"
IF = "if"
CAR = "car"
CDR = "cdr"
CONS = "cons"
ATOM = "atom?"

SUBTRACT = "-"
LESSTHAN = "<"

QUASIQUOTE="quasiquote"
UNQUOTE="unquote"
SPLICE="splice"

PRINT = "printsym"
INPUT_CHAR = "inputchar"

GENSYM = "gensym"
GENSYM_ESCAPE = "#"

        
class Environment(object):
    """An environment of definitions in which to interpret a Lisp Expression"""
    def __init__(self, parent_environment, interpreter=None):
        """Initialises the environment with its parent and an empty set of definitions"""
        self.parent_environment = parent_environment
        if self.parent_environment and self.parent_environment.interpreter:
            self.interpreter = self.parent_environment.interpreter
        else:
            self.interpreter = interpreter
        self.definitions = {}

    def retrieve_definition(self, label):
        """Get definition of label in environment if it exists"""
        if label.value in self.definitions:
            return self.definitions[label.value]
        elif self.parent_environment:
            return self.parent_environment.retrieve_definition(label)
        else:
            # If we have no definition then raise an error
            raise UnknownLabelError(label.value)

    def define(self, label, value):
        """Define a new function or value"""
        self.definitions[label.value] = value

    def create_child(self):
        return Environment(self)

    def __repr__(self):
        """Useful for debugging."""
        form_str = "{:^100}"
        if type(self) == DefaultEnvironment:
            return form_str.format("{DEFAULT ENVIRONMENT}")
        return (
            "" if self.parent_environment is None else ("{}\n"+form_str+"\n"+form_str).format(
            self.parent_environment, "||", str({
                k:repr(v) for k,v in self.definitions.items()}))) 
    

class DefaultEnvironment(Environment):
    """A default environment, with axiomatic function definitions"""
    def __init__(self, interpreter=None):
        """Creates the default environment"""
        super(DefaultEnvironment, self).__init__(None, interpreter)
        self._define_defaults()

    def _define_defaults(self):
        """Defines default functions"""
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

    def _define_defmacro(self):
        """Defines the defmacro function"""
        def defmacro(args, env):
            if len(args) < 3:
                raise WrongNumParamsError(DEFMACRO, 3, len(args))
            else:
                env.define(
                    args[0], MacroExpression(args[1:], args[0]))
                return args[0]
        self.define(SymbolExpression(DEFMACRO), LispFunction(DEFMACRO, defmacro))

    def _define_lambda(self):
        """Defines the lambda function"""
        self.define(SymbolExpression(LAMBDA), LispFunction(
            LAMBDA,
            lambda args, env : LambdaExpression(args, env)))

    def _define_quote(self):
        """Defines the quote function"""
        self.define(SymbolExpression(QUOTE), LispFunction(QUOTE, lambda args, env : args[0]))


    def _define_quasiquote(self):
        """Defines backquote"""
        def quasiquote(args, env):
            """Backquote function for quasiquote read macro"""
            if len(args) < 1:
                raise WrongNumParamsError(QUASIQUOTE, 1, len(args))
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
                    # From Bawden "Intuitively, an atsign has the eect of causing the
                    # comma to be mapped over the elements of the value
                    # of the following expression.
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


        self.define(SymbolExpression(QUASIQUOTE), LispFunction(QUASIQUOTE, quasiquote))

        
    def _define_if(self):
        """Defines the if 'function'"""
        def if_ex(args, env):
            if len(args) < 2:
                raise WrongNumParamsError(IF, 2, len(args))
            else:
                condition = args[0].evaluate(env)
                if condition.is_nill():
                    if len(args) > 2:
                        return args[2].evaluate(env)
                    else:
                        return Nils.nil
                else:
                    return args[1].evaluate(env)
        self.define(SymbolExpression(IF), LispFunction(IF, if_ex))

    def _define_atom(self):
        """Defines the atom function. True if argument is an atom, else ()"""
        def atom(args, env):
            if len(args) < 1:
                raise WrongNumParamsError(ATOM, 1, len(args))
            else:
                arg = args[0].evaluate(env)
                return arg if arg.atom() else Nils.nil
        self.define(
            SymbolExpression(ATOM), LispFunction(
                ATOM, atom))

    def _define_car(self):
        """Defines the car function"""
        def car(args, env):
            if len(args) < 1:
                raise WrongNumParamsError(CAR, 1, len(args))
            exp = args[0].evaluate(env)
            try:
                return exp.car()
            except AttributeError:
                raise TypeError(CAR, exp, "List or Symbol")

        self.define(SymbolExpression(CAR), LispFunction(CAR, car))

    def _define_cdr(self):
        """Defines the cdr function"""
        def cdr(args, env):
            if len(args) < 1:
                raise WrongNumParamsError(CDR, 1, len(args))
            exp = args[0].evaluate(env)
            try:
                return exp.cdr()
            except AttributeError:
                raise TypeError(CDR, exp, "List or Symbol")
        self.define(SymbolExpression(CDR), LispFunction(CDR, cdr))

    def _define_cons(self):
        """Defines the cons function"""
        def cons(args, env):
            if len(args) < 2:
                raise WrongNumParamsError(CONS, 2, len(args))
            expr1 = args[0].evaluate(env)
            expr2 = args[1].evaluate(env)
            try:
                return expr2.cons(expr1)
            except AttributeError:
                raise TypeError(CONS, exp_2, "List or Symbol")
                
        self.define(SymbolExpression(CONS), LispFunction(CONS, cons))

    def _define_subtract(self):
        """Defines the subtract function"""        
        def subtract(args, env):
            if len(args) < 2:
                raise WrongNumParamsError(SUBTRACT, 2, len(args))
            expr1 = args[0].evaluate(env)
            expr2 = args[1].evaluate(env)
            if not type(expr1) == type(expr2) == NumberExpression:
                raise TypeError(SUBTRACT, "{} and {}".format(expr1, expr2), "Numbers")
            else:
                return expr1.subtract(expr2)
        self.define(SymbolExpression(SUBTRACT), LispFunction(SUBTRACT, subtract))

    def _define_lessthan(self):
        """Defines the less than function"""        
        def lessthan(args, env):
            if len(args) < 2:
                raise WrongNumParamsError(LESSTHAN, 2, len(args))
            expr1 = args[0].evaluate(env)
            expr2 = args[1].evaluate(env)
            try:
                return expr1.lessthan(expr2)
            except AttributeError:
                raise TypeError(LESSTHAN, "{} and {}".format(expr1, expr2), "Numbers or symbols")

        self.define(SymbolExpression(LESSTHAN), LispFunction(LESSTHAN, lessthan))

    def _define_print_sym(self):
        """Defines print function"""
        def print_sym(args, env):
            if len(args) < 1:
                raise WrongNumParamsError(PRINT, 1, len(args))
            # Add any newline characters (\n) or spaces (\s)
            print_val = args[0].evaluate(env)
            if type(print_val) not in {str, int, float, SymbolExpression, NumberExpression}:
                raise TypeError(PRINT, type(print_val), "Symbol")
            print_val = str(print_val)
            print_val = print_val.replace("\\n", "\n").replace("\\s", " ")
            print(print_val, end="")
            sys.stdout.flush()
            return Nils.null # Return an empty string
        self.define(SymbolExpression(PRINT), LispFunction(PRINT, print_sym))

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
            if len(ch) == 1 and ord(ch) == 13:
                ch = "\\n"
                print("\n", end="")
            elif ch == " ":
                ch = "\\s"
                print(" ", end="")
            else:
                print(ch, end="")
            sys.stdout.flush()
            return ch

        self.define(SymbolExpression(INPUT_CHAR), LispFunction(INPUT_CHAR, input_char))

    def _define_gensym(self):
        """Defines the gensym function to generate a symbol that doesn't yet exist"""
        counter = 0
        def gensym(args, env):
            nonlocal counter
            counter += 1
            return SymbolExpression(GENSYM_ESCAPE + str(counter))
        self.define(SymbolExpression(GENSYM), LispFunction(GENSYM, gensym))
