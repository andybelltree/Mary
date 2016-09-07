"""
Defines Lisp Expression objects to be evaluated. Basic hierarchy of inheritance is as
follows:

                          +---------------+
                          |LispExpression |
                          +--------+------+
            +----------------^     ^    ^----------------+
            |                      |                     |
    +-------+--------+     +-------+------+    +---------+--------------+
    |SymbolExpression|     |ListExpression|    |ApplicableLispExpression|
    +--+-------------+     +--------------+    +------------------------+
       ^   ^-------------+                    +-^         ^  ^------------------+
       |                 |                    |           |                     |
+------+-------+ +-------+---------+ +--------+------+ +--+-------------+ +-----+------+
|AtomExpression| |NumberExpression | |MacroExpression| |LambdaExpression| |LispFunction|
+--------------+ +-----------------+ +---------------+ +----------------+ +------------+

"""

from abc import ABCMeta, abstractmethod
from .LispErrors import *
from .EvalHistory import EvalHistory

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
        self.eval_hist = EvalHistory(self)
        
    def evaluate(self, environment):
        """By default, autoquote and return this expression"""
        return self

    def is_nill(self):
        """Returns true iff expression has a false value (empty list or empty string)"""
        for n in Nils.nils:
            if self.value == n.value:
                return True
        return False

    def is_empty(self):
        """Returns true iff expression is an empty list"""
        return self.value == Nils.nil.value

    def atom(self):
        """Returns true iff expression is a number or a symbol expression"""
        return False

    def is_macro(self):
        """Returns true iff expression is a macro expression. For stack tracing."""
        return False
    
    def copy(self):
        """Return a copy of this expression. Only needed for tracing back debugging."""
        return self.__class__(self.value)

    def track_children(self, children):
        self.eval_hist.add_children(children)

    def track_result(self, result):
        self.eval_hist.add_result(result)
        return result

    def track_env(self, env):
        self.eval_hist.add_env(env)
        return env
        
    @staticmethod
    def create_atom(value):
        if type(value) in {int, float}:
            return NumberExpression(value)
        else:
            atom = SymbolExpression(value)
            try:
                return NumberExpression(int(atom.formatted()))
            except ValueError:
                try:
                    return NumberExpression(float(atom.formatted()))
                except ValueError:
                    return atom


class AtomExpression(LispExpression):
    """An atom expression"""
    def __init__(self, value):
        super(AtomExpression, self).__init__(value)

    def cons(self, other):
        """Join two atoms together"""
        return LispExpression.create_atom(str(other) + str(self))

    def car(self):
        """Return the first character of this atom"""
        return LispExpression.create_atom(str(self.value)[0]) if len(str(self.value)) > 0 else Nils.null

    def cdr(self):
        """Return everything but the first character of this symbol"""
        return LispExpression.create_atom(str(self.value)[1:])

    def lessthan(self, other):
        """Return self if this value is lexically less than the other 
        or the other is of a different type otherwise ()"""
        return Nils.null if type(other) == self.__class__ and self.value >= other.value else self

    def formatted(self):
        return str(self).replace("\\n", "\n").replace("\\s", " ")

    def atom(self):
        """Returns true as expression is an atom"""
        return True
    
    def __repr__(self):
        return str(self.value)

    
class SymbolExpression(AtomExpression):
    """A symbol. (Any atom which isn't a number)."""
    def evaluate(self, environment):
        """Look up the value in the environment and return"""
        return self.track_result(environment.retrieve_definition(self).copy())

class NumberExpression(AtomExpression):
    """A number"""
    def subtract(self, other):
        """Return the difference between this number and another"""
        return NumberExpression(self.value - other.value)

class ApplicableLispExpression(LispExpression):
    """A lisp expression which can be applied to a set of arguments"""
    def __init__(self, value, variable_param=None):
        super(ApplicableLispExpression, self).__init__(value)
        if len(value) != 2:
            raise WrongNumParamsError(type(self), 2, len(value))
        try:
            assert type(value[0]) == ListExpression
        except AssertionError:
            raise TypeError(LAMBDA, value[0], ListExpression)
        self.params = value[0].value
        self.body = value[1]
        self.variable_param = self._get_variable_param()

    def num_params(self):
        return len(self.params) + (1 if self.variable_param else 0)
        
    def _get_variable_param(self):
        """Get the variable parameter by finding it in the
        list of parameters and remove it from the list of
        parameters. Return None if not found"""
        for i in range(len(self.params)):
            if self.params[i].value == "&rest":
                if i + 2 > len(self.params):
                    raise KeywordError(self.params[i], "Must be followed by a parameter")
                variable_param = self.params[i + 1]
                # Remove the keyword and variable parameter from the list of parameters
                self.params = self.params[:i] + self.params[i+2:]
                return variable_param

    def _define_args(self, arguments, env):
        for i in range(len(self.params)):
            env.define(self.params[i], arguments.value[i])
        if self.variable_param:
            # Put any extra parameters into the variable parameter
            env.define(
                self.variable_param,
                ListExpression(arguments.value[len(self.params):]))

    def check_args(self, arguments):
        """Check the right number of arguments have been passed"""
        if len(arguments.value) < len(self.params) or len(
                arguments.value) > len(self.params) and not self.variable_param:
            raise(WrongNumParamsError(self, len(self.params), len(arguments.value)))

    def params_str(self):
        return "(" + " ".join([repr(p) for p in self.params] + (
            [repr(self.variable_param)] if self.variable_param else [])) + ")"

    @abstractmethod
    def apply_to(self, arguments, environment):
        """Apply expression to arguments"""
        raise NotImplementedError

class LispFunction(ApplicableLispExpression):
    """A  built in lisp function"""
    def __init__(self, name, definition, num_expected_args=None):
        super(LispFunction, self).__init__([Nils.nil, definition], None)
        self.num_expected_args = num_expected_args
        self.name = str(name)
        
    def apply_to(self, args, environment, caller):
        """Call function on arguments in environment. Eval info is stored with caller for
        debugging purposes"""
        if self.num_expected_args and len(args.value) < self.num_expected_args:
            raise WrongNumParamsError(self.name, self.num_expected_args, len(args.value))
        return caller.track_result(self.body(args.value, environment))

    def copy(self):
        """Create a copy of this function"""
        return self.__class__(self.name, self.body, self.num_expected_args)

    def __repr__(self):
        return "{} [built-in]".format(self.name)

class LambdaExpression(ApplicableLispExpression):
    """An anonymous function definition"""
    def __init__(self, value, parent_environment, variable_param = None):
        super(LambdaExpression, self).__init__(value, variable_param)
        self.environment = parent_environment

    def apply_to(self, arguments, environment, caller):
        """Apply to arguments in environment. Eval info stored with caller for debugging"""
        # Arguments are evaluated first
        arguments = ListExpression(
            [argument.evaluate(environment) for argument in arguments.value])
        # Ensure the right number of arguments were passed 
        self.check_args(arguments)
        # Copy the body to separate it during debugging. Keep track of the environment
        body = caller.track_result(self.body.copy())
        # Create a closure to apply the lambda in
        closure = body.track_env(self.environment.create_child())
        # Define all arguments in the closure
        self._define_args(arguments, closure)
        # Evaluate the body in the environment
        return body.evaluate(closure)

    def copy(self):
        """Create a copy of this lambda expression"""
        return self.__class__(self.value, self.environment, self.variable_param)

    def __repr__(self):
        return "(lambda {} {})".format(
            self.params_str(),
            self.body
        )

class MacroExpression(ApplicableLispExpression):
    """A macro. These will be applied to their arguments before their arguments are evaluated, then
    the result will be interpreted"""
    def __init__(self, name, value, variable_param = None):
        super(MacroExpression, self).__init__(value, variable_param)
        self.name = name

    def apply_to(self, arguments, environment, caller):
        """Apply to arguments in environment. Eval info stored with caller for debugging"""
        self.check_args(arguments)
        # Create an environment to apply the macro in
        env = self.track_env(environment.create_child())
        # Define parameters in environment
        self._define_args(arguments, env)
        # Evaluate the macro to get code to interpret, then interpret that code
        # Expand in a child environment of the one passed
        return caller.track_result(self.body.evaluate(env)).evaluate(environment)

    def is_macro(self):
        return True

    def copy(self):
        """Create a copy of this macro expression"""
        return self.__class__(self.name, self.value, self.variable_param)

    def __repr__(self):
        return "(macro {} {} {})".format(self.name,
            self.params_str(),
            self.body
        )
    
class ListExpression(LispExpression):
    """A List"""
    def __init__(self, input_list):
        assert(type(input_list) == list)
        super(ListExpression, self).__init__(input_list)
        self.track_children(self.value)

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
            fn = self.value[0].evaluate(environment)
            if issubclass(type(fn), ApplicableLispExpression):
                return fn.apply_to(ListExpression(self.value[1:]), environment, self)
            else:
                raise NotAFunctionError(fn)
            
    def __repr__(self):
        return "{}{}".format(TRANSLATIONS[self.value[0].value], repr(self.value[1])) if len(
            self.value) > 1 and self.value[0].atom() and self.value[
                0].value in TRANSLATIONS else "({})".format(" ".join([repr(i) for i in self.value]))

    def copy(self):
        """Return a copy of this list, with all items in it also copied"""
        return self.__class__([v.copy() for v in self.value])

class Nils(object):
    """All representations of False"""
    nil = ListExpression([])
    null = SymbolExpression("")
    nils = [nil, null]
