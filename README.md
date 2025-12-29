# Lambda Stuff

A simple lambda calculus interpreter with eager evaluation. This language focuses on the essentials of lambda calculus with minimal syntax.

## Features

- **Eager evaluation** - All expressions are evaluated immediately
- **Simple arithmetic** - Basic operations with numbers
- **Lambda functions** - First-class functions with closures
- **Variable assignment** - Mutable state for variables
- **REPL mode** - Interactive development environment
- **File execution** - Run programs from files
- **Error reporting** - Detailed parse and evaluation errors with source context

## Installation

### From Source

```bash
git clone <repository-url>
cd lambda-stuff
cabal build
cabal install
```

### Using Cabal Run

```bash
git clone <repository-url>
cd lambda-stuff
cabal run lambda-stuff
```

## Usage

### REPL Mode

Start the interactive REPL:

```bash
lambda-stuff
# or
cabal run lambda-stuff
```

In the REPL:
- Enter expressions and press Enter to evaluate
- Empty line exits the REPL
- Variables persist across expressions

### File Execution

Run a program from a file:

```bash
lambda-stuff program.lambda
# or
cabal run lambda-stuff -- program.lambda
```

File format:
- One expression per line
- Lines starting with `#` are comments
- Empty lines are ignored
- Variables persist across the entire file

### REPL After Loading File

Load a file and start REPL with its environment:

```bash
lambda-stuff --repl program.lambda
# or
lambda-stuff program.lambda --repl
# or
cabal run lambda-stuff -- --repl program.lambda
# or
cabal run lambda-stuff -- program.lambda --repl
```

This loads all expressions from the file into the environment, then starts the REPL where you can interact with the defined variables and functions.

### Help

```bash
lambda-stuff --help
```

## Language Reference

### Basic Syntax

#### Variables
```lambda
x = 42
name = "hello"
```

#### Arithmetic Operations
```lambda
1 + 2 * 3    # => 7
10 - 4       # => 6
8 / 2        # => 4
5 * 3        # => 15
```

#### Lambda Functions
```lambda
# Function definition
add = /a /b a + b

# Function application
(add 1 2)    # => 3

# Anonymous functions
(/x x * 2) 5  # => 10
```

#### Function Calls
```lambda
# Define a function
square = /x x * x

# Call it
(square 5)   # => 25

# Chain calls
(add (square 3) (square 4))  # => 25
```

### Advanced Features

#### Closures
Functions capture their environment:

```lambda
# Create a function factory
make_adder = /n /x x + n
add_five = (make_adder 5)

# add_five remembers n = 5
(add_five 10)  # => 15
```

#### Recursion with Z Combinator

The Z combinator enables recursion:

```lambda
# Z combinator
Z = /f ((/x (f (/v ((x x) v)))) (/x (f (/v ((x x) v))))))

# Factorial function
fact = (Z (/self (/n 
  if n then n * (self (n - 1)) else 1
)))

# Use it
(fact 5)  # => 120
```

## Data Types

- **Numbers**: `42`, `3.14` (Word64 integers)
- **Strings**: `"hello"`, `"world"`
- **Functions**: `/x x + 1`, `/a /b a + b`
- **Void**: `()` - returned by expressions with no meaningful value

## Error Handling

The interpreter provides detailed error messages:

### Parse Errors
- Syntax errors with highlighted source
- Unexpected tokens and EOF
- Unclosed parentheses

### Evaluation Errors
- Division by zero
- Type errors (e.g., arithmetic on non-numbers)
- Unknown variables
- Function call errors

## Examples

### Simple Calculator
```lambda
# calculator.lambda
a = 10
b = 20
sum = a + b
diff = b - a
product = a * b
quotient = b / a
```

Run it:
```bash
lambda-stuff calculator.lambda
```

Output:
```
=> 10
=> 20
=> 30
=> 10
=> 200
=> 2
```

### Higher-Order Functions
```lambda
# Map-like function
map = /f /l 
  if l then (f (head l)) :: (map f (tail l)) else ()

# Double a list
double = /x x * 2
numbers = 1 :: 2 :: 3 :: ()
result = (map double numbers)
```

## Building Executables

### Development Build
```bash
cabal build
```

### Optimized Build
```bash
cabal build --enable-optimization
```

### Installation
```bash
cabal install
```

After installation, the executable will be available as `lambda-stuff` system-wide.

## File Extensions

Common file extensions for Lambda Stuff programs:
- `.lambda` - Standard extension
- `.l` - Short form
- `.lambda-stuff` - Full name

## Limitations

- No boolean type (use 0/1 for false/true)
- No built-in list operations (must implement manually)
- No modules or imports (single file programs)
- No standard library (must implement everything yourself)

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## License

GPL-2.0-or-later - See LICENSE file for details.