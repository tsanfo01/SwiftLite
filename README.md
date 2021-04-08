# SwiftLite
* Group members:
- Erica Zhang (ericazyzhang@gmail.com)
- Tim Sanford (tim.sanford23@gmail.com)

* This implementation of compiler can handle a simple program written in Swift Lite, it can currently print string literals. The string literals cannot have spaces in between (user can use underscore to replace spaces).

* The test script is written in python 3.7.

* To build the compiler:
make toplevel.native

* To run test script:
python3.7 script.py OR make test

* Validation:
The test script compares output in the terminal by running test file testprint.swl and the expected output, if there is no difference, then the program prints OK in the terminal.