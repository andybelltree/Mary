#!/usr/bin/env python3
"""
Takes a library of lisp function definitions and calculates interdependencies. 
Writes out to another file in topologically sorted order where one could 
remove the latter part of the file without affecting earlier parts

Usage:

calculate_dependencies.py <filein> <fileout>
"""

from lisp.parser import Parser
import sys
from collections import deque

def main(args):
    if len(args) < 3:
        print("Usage: {} <filein> <fileout>".format(args[0]))
        exit(0)
    try:
        filein = open(args[1], 'r')
    except IOError as error:
        print("Usage: {} <filein> <fileout>".format(args[0]))
        exit(0)
    src = filein.read()
    filein.close()
    ast = Parser(src, True).result()
    
    for definition in ast:
        FunctionDefinition(definition)
        
    FunctionDefinition.initialise_dependents()
    definitions = FunctionDefinition.all_defs()
        
    sorted_functions = deque()
    
    def visit(func):
        if not func.mark:
            for dependency in func.dependent_on:
                visit(dependency)
            func.mark = True
            sorted_functions.appendleft(func)

    for definition in definitions:
        visit(definition)

    try:
        fileout = open(args[2], 'w')
    except IOError as error:
        print("Usage: {} <filein> <fileout>".format(args[0]))
        exit(0)

    for definition in sorted_functions:
        definition.write_to_file(fileout)

    fileout.close()

class FunctionDefinition():
    """For storing a function definition"""
    all_definitions = {}
    def __init__(self, definition):
        self.name = str(definition.cdr().car())
        self.definition = definition
        self.find_dependencies()
        self.dependencies.discard(self.name)
        self.mark = False
        self.dependent_on = set()
        FunctionDefinition.all_definitions[self.name] = self

    def find_dependencies(self):
        arguments = set()
        dependencies = set()
        def find_dependencies_helper(expr):
            if not (expr.is_empty() or expr.atom()):
                if expr.car().atom():
                    dependency = str(expr.car())
                    dependencies.add(dependency)
                    find_argument_dependencies(expr.cdr())
                else:
                    find_argument_dependencies(expr.cdr())
                    find_dependencies_helper(expr.car())

        def find_argument_dependencies(expr):
            if not expr.is_empty():
                if expr.car().atom():
                    find_argument_dependencies(expr.cdr())
                else:
                    find_dependencies_helper(expr.car())
                    find_argument_dependencies(expr.cdr())

        def find_arguments(expr):
            if not expr.is_empty():
                arguments.add(str(expr.car()))
                find_arguments(expr.cdr())


        find_dependencies_helper(self.definition.cdr().cdr().cdr().car())
        find_arguments(self.definition.cdr().cdr().car())
        
        self.dependencies = dependencies.difference(arguments)

    @classmethod
    def all_defs(cls):
        return cls.all_definitions.values()
        
    @classmethod
    def get_func(cls, name):
        return cls.all_definitions.get(name, None)

    @classmethod
    def initialise_dependents(cls):
        for definition in cls.all_definitions.values():
            dependencies_list = list(definition.dependencies)
            for dependency in dependencies_list:
                dep_obj = cls.all_definitions.get(dependency, None)
                if dep_obj:
                    dep_obj.dependent_on.add(definition)
                else:
                    definition.dependencies.remove(dependency)


    def write_to_file(self, fileout):
        fileout.write(";; {} \n".format(self.name))
        fileout.write(";; DEPENDENCIES: {}\n".format(", ".join(list(self.dependencies)) if self.dependencies else "NONE"))
        fileout.write(";; DEPENDED ON BY: {}\n".format(
            ", ".join([func.name for func in self.dependent_on])  if self.dependent_on else "NONE"))
        fileout.write(str(self.definition) + "\n\n")

    
if __name__ == "__main__":
    main(sys.argv)
