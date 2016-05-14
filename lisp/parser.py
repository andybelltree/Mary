# Based heavily on parser.py by kvalle @
# https://github.com/kvalle/root-lisp/
import re
from .LispExpression import *

SPECIAL_SYNTAX = {
    "'":"quote",
    "`":"quasiquote",
    ",":"unquote",
    ",@":"splice",

}


COMMENT_CHAR = ";"
GENSYM_ESCAPE = "#"

class Parser(object):
    """Takes in a string representation of a Lisp Expression and returns a result
    compiled into a nested list structure"""
    def __init__(self, source, multiple=True):
        """Takes an input string and a boolean indicating whether multiple expressions
        are to be parsed"""
        self._init_special_characters()
        source = strip_comments(source)
        if len(source) > 0:
            if multiple:
                self._result = self._parse_multiple(source)
            else:
                self._result = self._parse(source)
        else:
            raise SyntaxError('No input')

    def _init_special_characters(self):
        """Initialise special characters in Lisp expressions"""
        self._syntax = SPECIAL_SYNTAX
        special_chars = ""
        for k in self._syntax:
            special_chars += k
        special_chars = "".join(set(special_chars))
        self._legal_atom_re = r"^[^\s){}]+".format(special_chars)

    def _parse(self, source):
        """Parses a single expression into the Abstract Syntax Tree"""
        exp, rest = self._partition_exp(source)
        if rest:
            raise SyntaxError('Expected EOF')
        if len(exp) > 1 and exp[:2] in self._syntax:
            return ListExpression([SymbolExpression(
                self._syntax[exp[:2]]), self._parse(exp[2:])])
        elif exp[0] in self._syntax:
            return ListExpression([SymbolExpression(
                self._syntax[exp[0]]), self._parse(exp[1:])])
        elif exp[0] == "(":
            end = self._find_matching_paren(exp)
            return ListExpression([self._parse(e) for e in self._split_exps(exp[1:end])])
        else:
            try:
                float(exp)
                return NumberExpression(exp)
            except ValueError:
                return SymbolExpression(exp)
            return exp

    def _parse_multiple(self, source):
        """Parses a set of Lisp expressions"""
        return [self._parse(e) for e in self._split_exps(source)]

    def _split_exps(self, source):
        """Splits an input string into subexpressions that can be parsed individually"""
        rest = source.strip()
        exps = []
        while rest:
            exp, rest = self._partition_exp(rest)
            exps.append(exp)
        return exps

    def _partition_exp(self, source):
        """
        Split string into (exp, rest) where exp is the first expression in the string
        and rest is the rest of the string
        """
        source = source.strip()
        if len(source) > 1 and source[:2] in self._syntax:
            exp, rest = self._partition_exp(source[2:])
            return source[:2] + exp, rest
        elif source[0] in self._syntax:
            exp, rest = self._partition_exp(source[1:])
            return source[0] + exp, rest
        elif source[0] == "(":
            end = self._find_matching_paren(source)
            return source[:end + 1], source[end + 1:]
        elif source[0] == GENSYM_ESCAPE:
            raise SyntaxError("'{}' reserved for gensyms".format(GENSYM_ESCAPE))
        else:
            match = re.match(self._legal_atom_re, source)
            if match is None:
                raise SyntaxError("Expected new expression but got \"{}...\". Do you have too many closing brackets?".format(source[:10]))
            end = match.end()
            atom = source[:end]
            return atom, source[end:]

    def _find_matching_paren(self, source, start=0):
        assert source[start] == '('
        pos = start
        open_brackets = 1
        while open_brackets > 0:
            pos += 1
            if len(source) == pos:
                raise SyntaxError("Unbalanced expression: {}".format(source[start:]))
            elif source[pos] == '(':
                open_brackets += 1
            elif source[pos] == ')':
                open_brackets -= 1
        return pos


    def result(self):
        return self._result

    def unparsed_result(self):
        return unparse(self._result)

def strip_comments(source):
    output = []
    for line in source.split("\n"):
        line = line.strip()
        next_line = ""
        for char in line:
            if char == COMMENT_CHAR:
                break
            else:
                next_line += char
        output.append(next_line)
    transform = "\n".join(output)
    return transform
    
def unparse(result):
    return str(result)
