"""Errors specific to Lisp interpretation"""

class LispError(Exception):
    """Generic Lisp interpretation error"""
    def __init__(self):
        self.msg = "Error in code"

class UnknownTypeError(LispError):
    """
    Exception raised for unknown types in Lisp expression interpretation
    """
    def __init__(self, obj):
        self.obj = obj
        self.msg = "Unable to interpret object: {}, unknown type: {}".format(
            self.obj, type(self.obj))

class UnknownLabelError(LispError):
    """
    Exception raised for unkown symbols in Lisp expression
    """
    def __init__(self, label):
        self.label = label
        self.msg = "Label {} is not defined".format(label)

class WrongNumParamsError(LispError):
    """
    Exception raised for not enough parameters being passed
    """
    def __init__(self, fnname, expected, got):
        self.expected = expected
        self.got = got
        self.msg = "{} expected {} parameters but got {}".format(fnname, expected, got)

class BadInputError(LispError):
    """
    Exception raised when input doesn't match what expected
    """
    def __init__(self, expected, got):
        self.expected = expected
        self.got = got
        self.msg = "Expected {}, but received {}".format(
            self.expected, self.got
        )

class CannotEvaluateError(LispError):
    """
    Exception raised when a given expression cannot be evaluated
    """
    def __init__(self, expression):
        self.expression = expression
        self.msg = "Cannot evaluate the following expression: {}".format(self.expression)

class NotAFunctionError(LispError):
    """
    Exception raised when a non-function is applied to some parameters
    """
    def __init__(self, expression):
        self.expression = expression
        self.msg = "Tried to invoke {0}, but this does not evaluate to a function".format(
            self.expression
        )

class KeywordError(LispError):
    """
    Exception raised by a keyword being used innappropriately
    """
    def __init__(self, keyword, info):
        self.msg = "Bad use of keyword: {}. {}".format(keyword, info)

class TypeError(LispError):
    """
    Expected object to evaluate to a different type
    """
    def __init__(self, fn, expression, expected_type):
        self.msg = "Bad type: {} expected {} but got {}".format(fn, expected_type, expression)
