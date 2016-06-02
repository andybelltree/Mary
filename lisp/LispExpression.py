from abc import ABCMeta, abstractmethod
from .LispErrors import *

QUOTE="quote"
QUASIQUOTE="quasiquote"
UNQUOTE="unquote"
SPLICE="splice"
TRANSLATIONS = {
    # For unparsing expressions
    QUOTE:"'",
    QUASIQUOTE:"`",
    UNQUOTE:",",
    SPLICE:",@"   
}
LAMBDA="lambda"

# Lisp Expression objects
class LispExpression(metaclass=ABCMeta):
    """A generic Lisp Expression"""
    def __init__(self, value):
        self.value = value
        
        # Following are for storing debug information
        self.children = []
        self.result = None
        self.eval_env = None

    @abstractmethod
    def evaluate(self, environment):
        raise NotImplementedError

    def is_nill(self):
        """Returns true iff expression has a false value (empty list, 0, or empty string)"""
        for n in Nils.nils:
            if self.value == n.value:
                return True
        return False

    def is_empty(self):
        """Returns true iff expression is an empty list"""
        return self.value == Nils.nil.value

    def atom(self):
        """Returns true iff expression is a number or a symbol expression"""
        return type(self) in {NumberExpression, SymbolExpression}

    def print_macros(self, depth):
        """Print all macro expansions done when evaluating this expression"""
        is_macro = len(self.children) > 0 and type(self.children[0]) == MacroExpression
        if is_macro:
            print("|" * depth)
            print(">" * depth + str(self))
            print("-" * depth + str(self.children[0]))
        for child in self.children:
            child.print_macros(depth + 1)
        if is_macro:
            print("=" * depth + str(self.result))
        if self.result:
            self.result.print_macros(depth)
    
    def print_eval(self, depth, verbose=False, char='-'):
        """Print all steps taken to evaluate this expression"""
        print(char + (char + "|") * (depth-1) + ">" + str(self))
        if verbose and self.eval_env:
            print("\n" + str(self.eval_env) + "\n")
        if self.result:
            for child in self.children:
                child.print_eval(depth + 1, verbose, char)
            self.result.print_eval(depth, verbose, "=" if char == '-' else '-')

    def copy(self):
        """Return a copy of this expression. Only needed for tracing back debugging."""
        return self.__class__(self.value)

class SymbolExpression(LispExpression):
    """A symbol"""
    def __init__(self, value):
        super(SymbolExpression, self).__init__(value)

    def evaluate(self, environment):
        """Look up the value in the environment and return"""
        self.result = environment.retrieve_definition(self)
        return self.result

    def lessthan(self, other):
        """Return self if this value is lexically less than the other 
        or the other is of a different type otherwise ()"""
        return Nils.null if type(other) == SymbolExpression and self.value >= other.value else self

    def car(self):
        """Return the first character of this symbol"""
        return SymbolExpression(self.value[0])

    def cdr(self):
        """Return everything but the first character of this symbol"""
        return SymbolExpression(self.value[1:])

    def cons(self, other):
        """Join two symbols together"""
        return SymbolExpression(other.value + self.value)

    def __repr__(self):
        return self.value

class NumberExpression(LispExpression):
    """A number"""
    def __init__(self, value):
        if type(value) in {int, float}:
            super(NumberExpression, self).__init__(value)
        else:
            try:
                super(NumberExpression, self).__init__(int(value))
            except ValueError:
                super(NumberExpression, self).__init__(float(value))
            except ValueError:
                raise BadInputError("number", value)

    def subtract(self, other):
        """Return the difference between this number and another"""
        return NumberExpression(self.value - other.value)

    def lessthan(self, other):
        """Return 1 iff this value is less than the other otherwise 0"""
        return Nils.nought if type(other) == NumberExpression and self.value >= other.value else NumberExpression(1)
            
    def evaluate(self, environment):
        """Autoquote (don't evaluate)"""
        return self

    def __repr__(self):
        return str(self.value)

class ApplicableLispExpression(LispExpression):
    """A lisp expression which can be applied to a set of arguments"""
    def __init__(self, value):
        super(ApplicableLispExpression, self).__init__(value)
        if len(value) != 2:
            raise WrongNumParamsError(type(self), 2, len(value))
        try:
            assert type(value[0]) == ListExpression
        except AssertionError:
            raise TypeError(LAMBDA, value[0], ListExpression)
        self.params = value[0].value
        self.body = value[1]
        self.num_params = len(self.params)

    @abstractmethod
    def apply_to(self, arguments, environment):
        """Apply expression to arguments"""
        raise NotImplementedError

class LispFunction(ApplicableLispExpression):
    """A lisp function"""
    def __init__(self, name, definition):
        super(LispFunction, self).__init__([Nils.nil,definition])
        self.name = name
        
    def apply_to(self, args, environment, caller):
        """Call function on arguments in environment. Eval info is stored with caller for
        debugging purposes"""
        caller.children = args.value
        caller.result = self.body(args.value, environment)
        return caller.result

    def evaluate(self, environment):
        """Autoquote"""
        return self

    def copy(self):
        """Create a copy of this function"""
        return self.__class__(self.name, self.body)

    def __repr__(self):
        return self.name

class LambdaExpression(ApplicableLispExpression):
    """An anonymous function definition"""
    def __init__(self, value, parent_environment):
        super(LambdaExpression, self).__init__(value)
        self.environment = parent_environment

    def evaluate(self, environment):
        """Autoquote"""
        return self

    def apply_to(self, arguments, environment, caller):
        """Apply to arguments in environment. Eval info stored with caller for debugging"""
        # Arguments are evaluated first
        caller.children = arguments.value
        arguments = ListExpression(
            [argument.evaluate(environment) for argument in caller.children])
        
        if len(arguments.value) < self.num_params:
            raise(WrongNumParamsError(LAMBDA, self.num_params, len(arguments)))
        
        # Create a closure to apply the lambda in
        closure = self.environment.create_child()

        # Copy the body to separate it during debugging. Keep track of the environment
        caller.result = self.body.copy()
        caller.result.eval_env = closure

        # Define all arguments in the closure
        for i in range(self.num_params):
            closure.define(self.params[i], arguments.value[i])

        # Evaluate the body in the environment
        result = caller.result.evaluate(closure)

        return result

    def copy(self):
        """Create a copy of this lambda expression"""
        return self.__class__(self.value[:], self.environment)

    def __repr__(self):
        return "(lambda ({}) {})".format(
            " ".join([repr(p) for p in self.params]),
            self.body
        )



class MacroExpression(ApplicableLispExpression):
    """A macro"""
    def __init__(self, value, name, variable_param = None):
        super(MacroExpression, self).__init__(value)
        # Name is kept for debugging purposes
        self.name = name
        # Need to extract any keyword arguments (&rest)
        self.variable_param = variable_param if variable_param else self._get_variable_param()

    def _get_variable_param(self):
        """Get the variable parameter by finding it in the
        list of parameters and remove it from the list of
        parameters. Return None if not found"""
        for i in range(self.num_params):
            if self.params[i].value == "&rest":
                if i + 2 > self.num_params:
                    raise KeywordError(self.params[i], "Must be followed by a parameter")
                variable_param = self.params[i + 1]
                # Remove the keyword and variable parameter from the list of parameters
                self.params.pop(i)
                self.params.pop(i)
                self.num_params -= 2
                return variable_param

    def evaluate(self, environment):
        """Autoquote"""
        return self

    
    def apply_to(self, arguments, environment, caller):
        """Apply to arguments in environment. Eval info stored with caller for debugging"""
        caller.children = [self] + arguments.value
        # Create an environment to apply the macro in
        env = environment.create_child()
        # Keep track of environment for debugging
        self.eval_env = env
        
        if len(arguments.value) < self.num_params:
            raise(WrongNumParamsError(self, self.num_params, len(arguments)))

        # Define parameters in environment
        for i in range(self.num_params):
            env.define(self.params[i], arguments.value[i])
            
        if self.variable_param:
            # Put any extra parameters into the variable parameter
            env.define(
                self.variable_param,
                ListExpression(arguments.value[self.num_params:]))
            
        # Evaluate the macro to get code to interpret, then interpret that code
        # Expand in a child environment of the one passed
        caller.result = self.body.evaluate(env)
        # Interpret in the passed environment
        return caller.result.evaluate(environment)


    def copy(self):
        """Create a copy of this macro expression"""
        return self.__class__(self.value[:], self.name, self.variable_param)

    def __repr__(self):
        return "(macro {} ({}) ({}))".format(self.name,
            " ".join([repr(p) for p in self.params] + ([repr(self.variable_param)] if self.variable_param else [])),
            self.body
        )
    
class ListExpression(LispExpression):
    """A List"""
    def __init__(self, input_list):
        assert(type(input_list) == list)
        super(ListExpression, self).__init__(input_list)

    def car(self):
        """First item in list"""
        return self.value[0] if len(self.value) > 0 else Nils.nil

    def cdr(self):
        """Everything but first item in list"""
        return ListExpression(self.value[1:])

    def cons(self, other):
        """Append an item to the front of this list"""
        return ListExpression([other] + self.value)
        
    def evaluate(self, environment):
        """Apply the first item of the list to the rest of the list"""
        if len(self.value) == 0:
            return self
        else:
            # Get the first item and apply it to the  rest
            fn = self.value[0].evaluate(environment)
            if issubclass(type(fn), ApplicableLispExpression):
                return fn.apply_to(ListExpression(self.value[1:]), environment, self)
            else:
                raise NotAFunctionError(fn)
            
    def __repr__(self):
        if len(self.value) > 1 and self.value[0].atom() and self.value[0].value in TRANSLATIONS:
                # If the first value is a quote, then unparse
                return "{}{}".format(TRANSLATIONS[self.value[0].value], repr(self.value[1]))
        return "({})".format(" ".join([repr(i) for i in self.value]))

    def copy(self):
        """Return a copy of this list, with all items in it also copied"""
        return self.__class__([v.copy() for v in self.value])

class Nils(object):
    """All representations of False"""
    nil = ListExpression([])
    nought = NumberExpression(0)
    null = SymbolExpression("")
    nils = [nil, nought, null]
