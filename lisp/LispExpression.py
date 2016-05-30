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
        self.children = []
        self.result = None

    @abstractmethod
    def evaluate(self, environment):
        raise NotImplementedError

    def is_nill(self):
        for n in Nils.nils:
            if self.value == n.value:
                return True
        return False

    def is_empty(self):
        return self.value == []

    def atom(self):
        return type(self) in {NumberExpression, SymbolExpression}

    def print_eval(self, depth, char='-'):
        depth %= 40
        print(char*depth*2 + ">" + str(self))
        if self.result:
            for child in self.children:
                child.print_eval(depth + 1, char)
            self.result.print_eval(depth, "=" if char == '-' else '-')

class SymbolExpression(LispExpression):
    """A symbol"""
    def __init__(self, value):
        super(SymbolExpression, self).__init__(value)

    def evaluate(self, environment):
        """Look up the value in the environment and return"""
        self.result = environment.retrieve_definition(self)
        return self.result

    def lessthan(self, other):
        """Return 't iff this value is lexically less than the other otherwise ()"""
        return Nils.null if type(other) == SymbolExpression and self.value >= other.value else self

    def car(self):
        """Return the first character of this symbol"""
        return SymbolExpression(self.value[0])

    def cdr(self):
        """Return everything but the first character of this symbol"""
        if len(self.value) == 1:
            return Nils.null
        else:
            return SymbolExpression(self.value[1:])

    def cons(self, other):
        if type(other) != SymbolExpression:
            raise TypeError(CONS, "String", type(other))
        return SymbolExpression(repr(other) + self.value)

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
        """Return 'autoquote'"""
        return self

    def __repr__(self):
        return str(self.value)

class ApplicableLispExpression(LispExpression):
    """A lisp expression which can be applied to a set of arguements"""
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
        
    def apply_to(self, args, environment, parent):
        parent.result = self.body(args.value, environment)
        return parent.result

    def evaluate(self, environment):
        return self

    def __repr__(self):
        return self.name#"<builtin {} function>".format(self.name)

class LambdaExpression(ApplicableLispExpression):
    """An anonymous function definition"""
    def __init__(self, value, parent_environment):
        super(LambdaExpression, self).__init__(value)
        self.environment = parent_environment

    def __repr__(self):
        return "(lambda ({}) {})".format(
            " ".join([repr(p) for p in self.params]),
            self.body
        )

    def evaluate(self, environment):
        return self

    def apply_to(self, arguments, environment, parent):
        # Arguments are evaluated first
        arguments = ListExpression([argument.evaluate(environment) for argument in arguments.value])
        parent.children = arguments.value
        parent.result = self.body
        
        environment.interpreter.print_debug("Evaluating {} with arguments: {}", self, arguments)
        if len(arguments.value) < self.num_params:
            raise(WrongNumParamsError(LAMBDA, self.num_params, len(arguments)))
        
        # Create a closure to apply the lambda in
        closure = self.environment.create_child()

        for i in range(self.num_params):
            closure.define(self.params[i], arguments.value[i])

        environment.interpreter.print_debug("Created temp env: \n{}", closure)
        
        result = self.body.evaluate(closure)

        environment.interpreter.print_debug("Evaluated {} with arguments: {} to {}", self, arguments, result)
        return result


class MacroExpression(ApplicableLispExpression):
    """A macro"""
    def __init__(self, value, name, variable_param = None):
        super(MacroExpression, self).__init__(value)
        # Name is kept for debugging purposes
        self.name = name
        # Need to extract any keyword arguments
        self.variable_param = variable_param
        for i in range(self.num_params):
            if self.params[i].value == "&rest":
                if i + 2 > self.num_params:
                    raise KeywordError(self.params[i], "Must be followed by a parameter")
                self.variable_param = self.params[i + 1]
                # Remove the keyword and variable parameter from the list of parameters
                self.params.pop(i)
                self.params.pop(i)
                self.num_params -= 2
                break

    def evaluate(self, environment):
        return self

    def __repr__(self):
        return "(macro {} ({}) ({}))".format(self.name,
            " ".join([repr(p) for p in self.params] + ([repr(self.variable_param)] if self.variable_param else [])),
            self.body
        )

    def apply_to(self, arguments, environment, parent):
        copy = MacroExpression(self.value[:], self.name, self.variable_param)
        return copy.invoke(arguments, environment, parent)

    def invoke(self, arguments, environment, parent):
        # Create an environment to apply the macro in
        env = environment.create_child()
        if len(arguments.value) < self.num_params:
            raise(WrongNumParamsError(self, self.num_params, len(arguments)))
        for i in range(self.num_params):
            env.define(self.params[i], arguments.value[i])
        if self.variable_param:
            # Put any extra parameters into the variable parameter
            env.define(
                self.variable_param,
                ListExpression(arguments.value[self.num_params:]))
        # Evaluate the macro to get code to interpret, then interpret that code
        # Expand in a child environment of the one passed
        environment.interpreter.print_macroexpansion(
            "MACROEXPANDING {}", ListExpression([self.name] + arguments.value))
        macro_expansion = self.body.evaluate(env)
        parent.result = macro_expansion
        environment.interpreter.print_macroexpansion(
            "MACROEXPANSION of {}:\n\t{}", ListExpression([self.name] + arguments.value), macro_expansion)
        # Interpret in the passed environment
        result = macro_expansion.evaluate(environment)
        return result
    
class ListExpression(LispExpression):
    """A List"""
    def __init__(self, input_list):
        assert(type(input_list) == list)
        super(ListExpression, self).__init__(input_list)

    def car(self):
        """First item in list"""
        if len(self.value) > 0:
            return self.value[0]
        else:
            return Nils.nil

    def cdr(self):
        """Everything but first item in list"""
        return ListExpression(self.value[1:])

    def cons(self, other):
        """Append an item to the front of this list"""
        return ListExpression([other] + self.value)
        
    def evaluate(self, environment):
        """Apply the first item of the list to the rest of the list"""
        #environment.interpreter.print_debug(
        #    "--evaluating: {} in environment: \n{}\n", self, environment)
        if len(self.value) == 0:
            return self
        else:
            # Get the first item and invoke it on the  rest
            self.children = self.value
            fn = self.children[0].evaluate(environment)
            if issubclass(type(fn), ApplicableLispExpression):
                arguments = ListExpression(self.children[1:])
                return fn.apply_to(arguments, environment, self)
            else:
                raise NotAFunctionError(fn)

            
    def __len__(self):
        return len(self.value)
        
    def __repr__(self):
        if len(self.value) > 1 and self.value[0].atom() and self.value[0].value in TRANSLATIONS:
                # If the first value is a quote, then unparse
                return "{}{}".format(TRANSLATIONS[self.value[0].value], repr(self.value[1]))
        return "({})".format(" ".join([repr(i) for i in self.value]))

class Nils(object):
    """All representations of False"""
    nil = ListExpression([])
    nought = NumberExpression(0)
    null = SymbolExpression("")
    nils = [nil, nought, null]
