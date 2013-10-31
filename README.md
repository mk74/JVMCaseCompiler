The Case Language Compiler to JVM's Bytecode

Package:
--------

The package includes this report file, the compiler's source code in Haskell ( CaseCompiler/compiler.hs ), jasmin( jvm assembler ) and my_makefile.sh . The last bash script compiles Haskell's compiler, runs it, generates jasmin source files, compiles them into Java class and runs them into JVM. To run the script, please invoke "./my_makefile.sh". 

The Case language and the features of the compiler:
--------------------------------------------------

The Case program consists of expressions, which include constants, case expressions, for-loops and type declarations. The compiler supports following features:
	-simple expressions
		e.g. 4 * 6, "something" == "something", 10 > 5
	-structured expressions 
		e.g. a = 10; b = 4; c = (a div 2) * (b + 3)
	-simple data structures
		e.g. type Time = Hour int | Min int; t = Min 2
	-nested data structures
		e.g. type Age = Age int; type Address = Addr int int; type Person = Person Age Address; kevin = Person {Age 21} { Addr 1 2}
	-basic case discrimination based on values
		e.g. case 1 of 0 -> 1 | 1 -> 0
	-advanced case discrimination based on pattern matching
		e.g. case kevin of 
				Person agek addr -> case vicki of
					Person agev addr -> agek + agev;

Under the hood:
---------------

The compiler is based on the following design decisions:

	1)one pass compilation
	2)data structure representation:
		All data structures are kept as the instances of the same algebraic data type class. This JVM class looks as follows in Java code:

		class Adt{
			String tag; //used for data construct "Person" in the above examples
			Int value; 
			Adt arr[];
		}

		By using this structure, nested data structures are effectively represented as trees. For example, Min 2
		could be created by following java code:

			Adt adt1 = new Adt();
			adt1.tagname = "Min"
			adt1.arr = Adt[1]
			adt1.arr[0] = new Adt();
			adt1.arr[0].tagname = "int";
			adt1.value = 2;

		An alternative approach would be to create separate class for each data structures. However, that seems to be overcomplicated and requires much more time to implement. Our ADT class may be expanded to accommodate other primitives by defining value field as Object and simply boxing primitives.

	3)tracing types of operands on stack
		This information is needed as the program is supposed to print the top element of the stack at the very end. The information is kept in compiler's environment. It is updated if specific instructions are added to bytecode (e.g. "sipush" changes compiler's state to "int"). This approach imposes limitations as compiler's knowledge is not enough for multi-branches programs. The information should be either stored and updated form final program's code or all values should be boxed. The first one could be implemented using class' static field and after specific instructions, there would be instructions updating this field ( e.g. "sipush 10 " followed by "iconst_0\n putstatic Program/flag I"). If there were all values boxed, the program could call "print" function for JVM's object. There were effort to detect what the type of top element is by trying to catch exceptions. However, those exceptions can't be caught by standard JVM's instructions. 

