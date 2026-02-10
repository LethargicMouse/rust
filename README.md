# Good language manual
Good is a strongly typed general-purpose programming language that is designed
- to allow high code reusability with
  - [generics](#generics)
  - [traits](#traits)
- to allow using thousands of libraries and embed Good programs in C projects with
  - [full C compatibility](#c-compatibility)
- to allow high level programming with 
  - [algebraic data types](#algebraic-data-types)
  - [type inference](#type-inference)
  - [RAII](#raii)
  - [custom coercion](#coercion)
  - [error system](#error-system)
  - [environments](#environments)
- to allow low level programming with
  - [manual memory management with raw pointers](#manual-memory-management)
  - [full C compatibility](#c-compatibility)

Basically the Good language allows everything one may need. The Good language is a good language.

## Basics
The Good program is a file that contains several `items`. An `item` can be
- a [function](#functions)
- an [extern declaration](#extern-declarations)
- a [struct](#structs)
- a [type alias](#type-aliases)
- a [trait](#traits)
- a [trait implementation](#traits)
- a [constant](#constants)

### Functions
All (or almost all) the Good code is contained in functions:
```good
fn one_two_three() {
  println("one...");
  println("two.....");
  println("three!");
}
```

Functions can be called from other functions:
```good
fn print_something() {
  one_two_three();
}
```

Functions with just one expression (like the previous one) can be written in a short form:
```good
fn print_something()
  do one_two_three()
```

The `main` function is special in that it is called at the beginning of the program and is required to be present in the code:
```good
fn main()
  do println("program started (and finished)")
```

Functions can accept parameters:
```good
fn print_sum(a: i32, b: i32)
  do println(a + b)
```
Here `i32` is a type of 32-bit integers.

To call such functions, one shall pass arguments of corresponding types to it:
```good
fn main()
  do print_sum(123, 321)
```

Functions can also return some result:
```good
fn add(a: i32, b: i32) i32
  do a + b
```
Here `i32` is a return type, and the last expression of a block or, in this case, the expression after `do` is returned.

The result of such functions can be used by the caller:
```good
fn main()
  do println(add(123, 321))
```

Functions with at least one parameter can also be called with "dot-notation":
```good
fn main()
  do add(123, 321).println()
```
Here the `println` function is called with "dot-notation", and `add(123, 321)` becomes its first argument.
This expression is equivalent to the previous one.

### Type Aliases
*work in progress*

### Constants
*work in progress*

## Generics
*work in progress*

## Traits
*work in progress*

## C Compatibility
*work in progress*

### Extern Declarations
*work in progress*

## Algebraic Data Types
*work in progress*

### Structs
*work in progress*

## Type Inference
*work in progress*

## RAII
*work in progress*

## Coercion
*work in progress*

## Error System
*work in progress*

## Environments
*work in progress*

## Manual Memory Management
*work in progress*
