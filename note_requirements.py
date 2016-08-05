from lisp.parser import Parser
import sys
from collections import deque

def main(args):
    try:
        filein = open(args[1], 'r')
        fileout = open(args[2], 'w')
    except Exception as e:
        print("Usage: {} <filein> <fileout>".format(args[0]))
        exit(0)
    src = filein.read()
    ast = Parser(src, True).result()
    definitions = set()
    for definition in ast:
        function_name = definition.cdr().car()
        function_def = definition.cdr().cdr().cdr().car()
        dependencies = find_dependencies(function_def)
        definitions.add(FunctionDefinition(str(function_name), definition, dependencies))
    FunctionDefinition.initialise_dependents()
        
    sorted_functions = deque()
    
    def visit(func):
        if not func.mark:
            for dependency in func.dependent_on:
                visit(dependency)
            func.mark = True
            sorted_functions.appendleft(func)

    for definition in list(definitions):
        visit(definition)
        
    for definition in sorted_functions:
        definition.write_to_file(fileout)

        

def find_dependencies(expr):
    if expr.is_empty() or expr.atom():
        return set()
    elif expr.car().atom():
        return set([str(expr.car())]).union(find_argument_dependencies(expr.cdr()))
    else:
        return find_dependencies(expr.car()).union(find_argument_dependencies(expr.cdr()))

def find_argument_dependencies(expr):
    if expr.is_empty():
        return set()
    if expr.car().atom():
        return find_argument_dependencies(expr.cdr())
    else:
        return find_dependencies(expr.car()).union(find_argument_dependencies(expr.cdr()))

class FunctionDefinition():
    all_definitions = {}
    def __init__(self, name, definition, dependencies):
        self.name = name
        self.definition = definition
        self.dependencies = dependencies
        self.dependencies.discard(self.name)
        self.mark = False
        self.dependent_on = set()
        FunctionDefinition.all_definitions[self.name] = self

    @classmethod
    def get_func(cls, name):
        return cls.all_definitions.get(name, None)

    @classmethod
    def initialise_dependents(cls):
        for definition in cls.all_definitions.values():
            for dependency in definition.dependencies:
                dep_obj = cls.all_definitions.get(dependency, None)
                if dep_obj:
                    dep_obj.dependent_on.add(definition)

    def write_to_file(self, fileout):
        fileout.write(";; {} \n".format(self.name))
        fileout.write(";; DEPENDENCIES: {}\n".format(", ".join(list(self.dependencies))))
        fileout.write(";; DEPENDED ON BY: {}\n".format(", ".join([func.name for func in self.dependent_on])))
        fileout.write(str(self.definition) + "\n\n")

    
if __name__ == "__main__":
    main(sys.argv)
