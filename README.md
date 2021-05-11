# SwiftLite
* Group members:
- Erica Zhang (ericazyzhang@gmail.com)
- Tim Sanford (tim.sanford23@gmail.com)



* The test script is written in python 3.7.

* To build the compiler:
make toplevel.native

* To compile a SwiftLite program:
python3.7 compile.py [filename]

This compiles the given SwiftLite program and produces an executable called 'test.exe'

* To run test script:
python3.7 script.py OR make test

* Validation:
The test script compares output in the terminal by running test file testprint.swl and the expected output, if there is no difference, then the program prints OK in the terminal.

* Tests
test-print.swl      - basic string printing - a hello world program

test-var.swl        - variable declaration and lookup

test-assign.swl     - variable assignment

test-let.swl        - constant variable declaration and lookup

test-if1.swl        - conditional branching w/ only true branch

test-if2.swl        - conditional branching w/ true and false

test-if3.swl        - conditional branching w/ only true branch and false condition

test-if4.swl        - conditional branching w/ true and false and false condition

test-while1.swl     - basic while loop

test-char.swl       - basic char printing

test-scope1.swl     - global variable accessible in a local scope

test-scope2.swl     - local variable accessible in sub scope

test-scope3.swl     - name redefined at different scoping levels uses most recent definition

test-scope4.swl     - global variable accessible in a function scope

test-scope5.swl     - function parameter takes precedence over global variable

test-scope6.swl     - local var takes precedence over function parameter and global

test-arrat1.swl     - basic array access

test-arrat2.swl     - array access through a while loop and changing variable

test-arrat3.swl     - array access with 2D arrays

test-arrassign1.swl - assignment to index of an array

test-arrassign2.swl - assignment to 2D array

test-emptyarr.swl   - indexing into empty arrays type-checks, even if operation not performed

test-func1.swl      - basic function definition and call

test-func2.swl      - checking function return value

test-func3.swl      - checking function parameter and return

test-func4.swl      - checking function definition with more than one parameter

test-func5.swl      - checking function that calls previously defined function

test-func6.swl      - checking more complex function body

test-func7.swl      - recursive function definition

test-for1.swl       - basic for loop with array literal

test-for2.swl       - basic for loop with variable and counter

test-for3.swl       - nesting for loops with 2D array

test-for4.swl       - name of variable in for loop takes precedence over global var

test-return.swl     - return statement at top level does not halt execution

test-opt1.swl       - basic optional unwrapping

test-opt2.swl       - if let skips then branch when optional is nil

test-opt3.swl       - optional unwrapping, only executes true branch

test-opt4.swl       - nil value, only executes else branch

test-opt5.swl       - name bound in if let takes precedence over global var

test-opt6.swl       - function returning optional value

test-opt7.swl       - name bound in if let NOT bound in else branch - can be redefined and reassigned

test-concat1.swl    - basic string concatenation w/ literals

test-concat2.swl    - string concatenation w/ variables

test-concat3.swl    - store result of string concatenation in a variable

test-concat4.swl    - concat with empty string

test-concat5.swl    - concat string passed to/returned from function

test-range1.swl     - open range with for loop

test-range2.swl     - closed range with for loop

test-range3.swl     - range with non-literal ends

test-enum1.swl      - enum definition and comparison

test-enum2.swl      - enum comparision with different case

test-enum3.swl      - enum case returned from function

test-class1.swl     - simple class definition with method calls

test-class2.swl     - self method calls within method defn

test-class3.swl     - self field accessed within method body

test-class4.swl     - return self, changes to one object affect all other references to it

test-class5.swl     - multiple inits

test-class6.swl     - multiple inits with different types of arguments, check unassigned fields

test-class7.swl     - class with an instance another class as one of its fields

test-class8.swl     - complex class definition - multiple methods, external functions dealing with instances

test-class9.swl     - simple LinkedList data structure

test-class10.swl    - method and function with the same name

test-class11.swl    - return self from init



fail-var1.swl       - type of assignment and declaration don't match

fail-var2.swl       - name already declared

fail-assign1.swl    - type of assignment and variable don't match

fail-assign2.swl    - assignment to constant variable

fail-if1.swl        - if condition not a Bool

fail-if2.swl        - undeclared variable in true block

fail-if3.swl        - undeclared variable in false block

fail-while1.swl     - while condition not a Bool

fail-while2.swl     - undeclared variable in while body

fail-scope1.swl     - look up variable no longer in scope

fail-scope2.swl     - attempt to use global definition in local scope where name is redefined

fail-arrat1.swl     - index into non array value

fail-arrat2.swl     - index is not an Int

fail-arrassign1.swl - assign to index of non array value

fail-arrassign2.swl - index is not an Int

fail-arrassign3.swl - type of assignment and array elements don't match

fail-func1.swl      - return types dont match

fail-func2.swl      - nothing can follow a return

fail-func3.swl      - more arguments than params in definition

fail-func4.swl      - argument and parameter types dont match

fail-func5.swl      - function not defined

fail-for1.swl       - expr in for loop must be an array

fail-for2.swl       - name cannot be redefined in body of the for loop

fail-for3.swl       - for variable cannot be reassigned

fail-opt1.swl       - Int and Int? not interchangeable - 4 must be wrapped as (4)?

fail-opt2.swl       - if let expects an expression with some optional type

fail-opt3.swl       - if let expects type given to name and type inside optional to match

fail-opt4.swl       - if let name cannot be redefined

fail-opt5.swl       - if let name cannot be reassigned

fail-enum1.swl      - duplicate enum cases        

fail-enum2.swl      - enum definitions with the same name

fail-enum3.swl      - access case of an enum that is not defined

fail-enum4.swl      - access case of an enum that is defined, but does not include that case

fail-enum5.swl      - enum with same name as already defined class

fail-enum6.swl      - identical cases of different enums are not equivalent

fail-class1.swl     - class definitions with the same name

fail-class2.swl     - class definition with the same name as already defined enum

fail-class3.swl     - bad field declaration

fail-class4.swl     - duplicate field declarations

fail-class5.swl     - bad method return

fail-class6.swl     - more than one nullary constructor

fail-class7.swl     - duplicate (non nullary) constructors

fail-class8.swl     - call method without using self

fail-class9.swl     - duplicate method definitions

fail-class10.swl    - recursive method call without self

fail-class11.swl    - bad return from init

fail-self1.swl      - self at toplevel

fail-self2.swl      - self field at toplevel

fail-self3.swl      - self method call at toplevel

