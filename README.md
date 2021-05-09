# SwiftLite
* Group members:
- Erica Zhang (ericazyzhang@gmail.com)
- Tim Sanford (tim.sanford23@gmail.com)

* This implementation of compiler can handle all statements, function definitions, and most expressions not relating to enums or classes (some binary operators (+ on strings, ... and ..<) have yet to be implemented).

* SYNTAX CHANGES
- String literals now accept any non-escape character (\ is just \ and a " always ends the string literal)
- Array literals now must be followed by a colon and the type of the array elements (eg. [1, 2, 3, 4] : Int or [] : String). This is to ease the type checking process, esp. for empty arrays
    - This complicates 2D arrays - [[1, 2, 3] : Int, [4, 5, 6] : Int, [7, 8, 9] : Int] : [Int]
- The anywhere where the nil literal is used as a value of the Optional type, it now must be followed by a colon and the type of the unwrapped optional (eg. nil : Int). This is to ease the type checking process and to ensure that a nil value is strongly typed.
- Anywhere that an expression of an Optional(t) type is expected (that is not the nil literal), it must be an expression of type t wrapped in parentheses and followed by a question mark (eg. (4)? for Int?). This is to differentiate between a value of type t and a value of type Optional(t)

* The test script is written in python 3.7.

* To build the compiler:
make toplevel.native

* To run test script:
python3.7 script.py OR make test

* Validation:
The test script compares output in the terminal by running test file testprint.swl and the expected output, if there is no difference, then the program prints OK in the terminal.

* Tests (There's more than 10, sorry)
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