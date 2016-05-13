from parser import Parser

to_parse = "(this is a '(test 2) see `(if ,(it)) works)"
p = Parser(to_parse, False)
print(p.result())
to_parse = "2"
p = Parser(to_parse, False)
print(type(p.result()))
to_parse = "hello"
p = Parser(to_parse, False)
print(type(p.result()))
to_parse = "(thuglife) watermelon 234 (and a (nested) list)"
p = Parser(to_parse, True)
print(p.result())

