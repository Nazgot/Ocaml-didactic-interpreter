# Ocaml-didactic-interpreter
This is a standard Ocaml interpreter. 
Int and Bool are primitive types.
Basic operations : + , - , * , / , And , Or, Eq (Eqals)
You can define unary functions and call them with the "Apply" construct, scoping is static.

# New Type: Tuple.
It is defined as a list of elements of different types such as Int Bool Fun

# Operation defined on Tuple
Pipe of Tuple * Exp, where exp is the parameter of the first function evaluated by Pipe.
Pipe [f1,f2,f3] 1 = f3(f2(f1(1)))

# Operation Manytimes
Manytimes of int * exp * exp  
Manytimes 3 f 1 = f(f(f(1)))

