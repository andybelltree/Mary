To run the repl, start ./mary
Mary will accept the following arguments:

-c Runs without the standard library
-v Gives verbose error reporting
-r <i> Sets maximum recursion depth to i (where i is an integer)
-m Outputs information about macroexpansion
-e Outputs information about the environment
-d Outputs evaluation history (Lisp stack trace)
-f <file_name> Interprets file instead of starting the repl
-a <macros|default|min> Chooses set of axioms to run on

The standard library is in stdlib.lisp. The original version of this library is in the lib/ folder, and stdlib.lisp is the result after running calculate_dependencies.py on this library. If you wish to remove any function definitions, take note of any dependencies it has and remove those too. To add function definitions, add them in the file in lib/, then run calculate_all_dependencies to update the base library.

To find information about different environments and sets of axioms, go to lisp/Environment.py. Instructions are there on how to navigate environments and build your own. If you create a new environment, you'll have to let the repl script know about it too. Make sure you give your library a set of standard libraries to read in as well.

To see how a LispExpression is stored and evaluated, go to lisp/LispExpression.py

There are some demo files to interpret in the demos/ folder.

Tests are in the tests/ folder. These are in files with lines of tests which take the form:
<expression> ; <expected result>
To run all the tests, use runtests.py
