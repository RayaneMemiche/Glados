## B-FUN-500 GLaDOS

B-FUN-500 GLaDOS is a Haskell-based programming project that aims to create a minimalist LISP interpreter. It provides features such as extensible syntax and grammar, enhanced semantic analysis, and optimized execution speed.

## Features

Minimalist LISP interpreter implementation.
Extensible syntax and grammar.
Enhanced semantic analysis.
Optimized execution speed.

### Installation

To use B-FUN-500 GLaDOS, follow these steps:

1. Clone the repository: `git clone git@github.com:EpitechPromo2026/B-FUN-500-PAR-5-2-glados-nicolas.poupon.git glados`.
2. Navigate to the project directory: `cd glados`.
3. Install dependencies: `stack setup`.

### Usage

To compile the interpreter, use the following command: `stack build`.
To run the interpreter, use the following command: `make`.
or run the interpreter using the following command: `stack exec glados`. Use standard LISP syntax for operations.
To run unit tests, use the following command: `stack test --coverage` or `make test_run`.

# Code Explanation

The code snippet provides examples of how to use the B-FUN-500 GLaDOS interpreter to perform basic arithmetic, define and use functions, and evaluate complex expressions in LUA.

## Example Usage

1. Basic Arithmetic
   - Example: Calculating the sum of two numbers in LUA.
   - Interaction: After starting the interpreter (`stack exec glados`), enter `5 + 3`.
   - Expected Output: The interpreter should display 8.

2. Function Definition and Use
   - Example: Defining and using function in LUA.
   - Interaction: Define a function in the interpreter by typing a LUA function definition, then call the function.
   - Expected Output: Output based on the defined function and its use.

3. Complex Expressions
   - Example: Evaluating nested expressions in LUA.
   - Interaction: Enter a nested expression like `(2 + 3) * 4`.
   - Expected Output: The interpreter should evaluate and display the result of the expression, such as 20.

## Code Analysis

### Inputs
- Basic Arithmetic: The input is a LUA arithmetic expression, e.g., `5 + 3`.
- Function Definition and Use: The input is a LUA function definition followed by its invocation.
- Complex Expressions: The input is a nested LUA expression, e.g., `(2 + 3) * 4`.

### Flow
1. Basic Arithmetic: The code snippet demonstrates how to use the interpreter to perform basic arithmetic operations by entering the LISP expression `(2 + 3)`. The interpreter should evaluate the expression and display the result, which is 5.
2. Function Definition and Use: The code snippet shows how to define a factorial function in the interpreter using a LUA expression. After defining the function, it can be used to calculate the factorial of a number, such as 5. The expected output is the factorial of the input number.
3. Complex Expressions: The code snippet illustrates how to evaluate nested expressions in the interpreter. By entering a nested expression like `((2 + 3) * 4)`, the interpreter should evaluate the expression and display the result, such as 20.

### Testing

To run unit tests, execute `stack test --coverag` in the project directory. This will run the tests defined in the `tests` directory.