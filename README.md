# Homework 7 (20 Points)

The deadline for Homework 7 is ~~Friday, November 9, 6pm~~ (extended:
Saturday, November 10, 6pm). The late
submission deadline is Thursday, October 15, 6pm.

## Getting the code template

Before you perform the next steps, you first need to create your own
private copy of this git repository. To do so, click on the link
provided in the announcement of this homework assignment on
Piazza. After clicking on the link, you will receive an email from
GitHub, when your copy of the repository is ready. It will be
available at
`https://github.com/nyu-pl-fa18/hw07-<YOUR-GITHUB-USERNAME>`.
Note that this may take a few minutes.

* Open a browser at `https://github.com/nyu-pl-fa18/hw07-<YOUR-GITHUB-USERNAME>` with your Github username inserted at the appropriate place in the URL.
* Choose a place on your computer for your homework assignments to reside and open a terminal to that location.
* Execute the following git command: <br/>
  ```git clone https://github.com/nyu-pl-fa18/hw07-<YOUR-GITHUB-USERNAME>.git```<br/>
  ```cd hw07```


## Preliminaries

We assume that you have installed a working OCaml distribution,
including the packages `ocamlfind`, `ocamlbuild`, and `ounit`. Follow
the instructions in the notes
for
[Class 7](https://github.com/nyu-pl-fa18/class07#installation-build-tools-and-ides) if
you haven't done this yet.

## Submitting your solution

Once you have completed the assignment, you can submit your solution
by pushing the modified code template to GitHub. This can be done by
opening a terminal in the project's root directory and executing the
following commands:

```bash
git add .
git commit -m "solution"
git push
```

You can replace "solution" by a more meaningful commit message.

Refresh your browser window pointing at
```
https://github.com/nyu-pl-fa18/hw07-<YOUR-GITHUB-USERNAME>/
```
and double-check that your solution has been uploaded correctly.

You can resubmit an updated solution anytime by reexecuting the above
git commands. Though, please remember the rules for submitting
solutions after the homework deadline has passed.

## Problem 1: Parametric Polymorphism (5 Points)

Write OCaml functions that satisfy the following polymorphic type signatures:

1. a function `f: ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c`

2. a function `g: ('a * 'b -> 'c) -> 'a -> 'b -> 'c`

3. a function `h: ('a -> 'b -> 'c) -> 'a * 'b -> 'c`

For the next two subproblems, assume we are additionally given the following algebraic data type:

```ocaml
type ('a,'b) sum = 
  | L of 'a 
  | R of 'b
```

4. a function `i: ('a, 'b) sum * ('a -> 'c) * ('b -> 'c) -> 'c`

5. a function `j: ('a, 'b * 'c) sum -> ('a, 'b) sum * ('a, 'c) sum`

It does not matter what your implementations of these functions do as
long as they satisfy the above type signatures. In all cases, there is
an obvious implementation that follows from the type signature. Note
that you are not allowed to use any explicit type annotations in your
solution.

**Hint:** Here is how to approach this problem. Consider the type of the function
`f`:

`f: ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c`

This type tells you that `f` is a curried function that takes three
parameters, say `x` `y` and `z`, and then returns some
result. So the basic shape of the definition of `f` must look like
this

```ocaml
let f x y z = e
```

for some appropriate expression `e` that you need to find. To figure
out what `e` is, inspect the type of `f` and what it tells you about
its parameters and return value. E.g. it tells you that `x` must be a
function of type `'a -> 'b`, `y` must be a function of type `'b -> 'c`
and `z` must be a value of type `'a`. Moreover, the return value has
type `'c`, which hence is the type of the expression `e` that you need
to find. So think about how to construct a value of type `'c` using
`x`, `y`, and `z`.

Put your solution into the file `src/problem1.ml`.

## Problem 2: MiniML

In this exercise we will practice the use of algebraic data types and
use it in an application domain that functional programming languages
are particularly well-suited for: symbolic computation.

The goal is to implement an interpreter for a dynamically typed subset
of OCaml, called *MiniML*. We will implement the interpreter in OCaml
itself. Most of the code will be given to you. Your task will be to
implement three functions that are missing in the given code. These
functions are relatively small but critical for the interpreter to
work correctly.

### Syntax

We consider a core language built around the untyped lambda
calculus. For convenience, we extend the basic lambda calculus with
primitive operations on Booleans and integers. We also introduce a fixpoint
operator to ease the implementation of recursive functions. The
concrete syntax of the language is described by the following grammar:

```
(Variables)
x: var     

(Integer constants)
i: int  ::= 
   ... | -1 | 0 | 1 | ...  

(Boolean constants)
b: bool ::= true | false   

(inbuilt functions)
f: inbuilt_fun ::=             
     not                   (logical negation)
   | fix                   (fixpoint operator)

(Binary infix operators)
bop: binop ::=             
     * | / | mod           (multiplicative operators)
   | + | - |               (additive operators)
   | && | ||               (logical operators)
   | = | <>                ((dis)equality operators)
   | < | > | <= | >=       (comparison operators)

(Terms)
t: term ::= 
     f                     (inbuilt functions)
   | i                     (integer constants)
   | b                     (Boolean constants)
   | x                     (variables) 
   | t1 t2                 (function application)
   | t1 bop t2             (binary infix operators)
   | if t1 then t2 else t3 (conditionals)
   | fun x -> t1           (lambda abstraction)
```

The rules for operator precedence and associativity are the
same [as in OCaml](https://caml.inria.fr/pub/docs/manual-ocaml/expr.html).

For notational convenience, we also allow OCaml's basic `let`
bindings, which we introduce as syntactic sugar. That is, the OCaml
expression

```ocaml
let x = t1 in t2
```

is syntactic sugar for the following term in our core calculus:

```ocaml
(fun x -> t2) t1
```

Similarly, the OCaml expression

```ocaml
let rec x = t1 in t2
```

is syntactic sugar for the term

```ocaml
(fun x -> t2) (fix (fun x -> t1))
```

We represent the core calculus of MiniML using algebraic data types
as follows:

```ocaml
(** source code position, line:column *)
type pos = { pos_line: int; pos_col: int }

(** variables *)
type var = string

(** inbuilt functions *)
type inbuilt_fun =
  | Fix (* fix (fixpoint operator) *)
  | Not (* not *)

(** binary infix operators *)
type binop =
  | Mult  (* * *)
  | Div   (* / *)
  | Mod   (* mod *)
  | Plus  (* + *)
  | Minus (* - *)
  | And   (* && *)
  | Or    (* || *)
  | Eq    (* = *)
  | Lt    (* < *)
  | Gt    (* > *)
  | Le    (* <= *)
  | Ge    (* >= *)

(** terms *)
type term =
  | FunConst of inbuilt_fun * pos      (* f (inbuilt function) *)
  | IntConst of int * pos              (* i (int constant) *)
  | BoolConst of bool * pos            (* b (bool constant) *)
  | Var of var * pos                   (* x (variable) *)
  | App of term * term * pos           (* t1 t2 (function application) *)
  | BinOp of binop * term * term * pos (* t1 bop t2 (binary infix operator) *)
  | Ite of term * term * term * pos    (* if t1 then t2 else t3 (conditional) *)
  | Lambda of var * term * pos         (* fun x -> t1 (lambda abstraction) *)
```

Note that the mapping between the various syntactic constructs and the
variants of the type `term` is fairly direct. The only additional
complexity in our implementation is that we tag every term with a
value of type `pos`, which indicates the source code position where
that term starts in the textual input given to our interpreter. We
will use this information to provide more meaningful error reporting
to the programmer.

### Code Structure, Compiling and Editing the Code, Running the Interpreter

The code template contains various OCaml modules that already
implement most of the functionality needed for our interpreter:

* [src/util.ml](src/util.ml): some useful utility
  functions and modules (the type `pos` is defined here)

* [src/ast.ml](src/ast.ml): definition of abstract syntax
  of MiniML (see above) and related utility functions

* [src/grammar.mly](src/grammar.mly): grammar definition
  for a parser that parses a MiniML term and converts it into an
  abstract syntax tree

* [src/lexer.mll](src/lexer.mll): associated grammar
  definitions for lexer phase of parser

* [src/parser.ml](src/parser.ml): interface to MiniML
  parser generated from grammar

* [src/eval.ml](src/eval.ml): the actual MiniML interpreter

* [src/miniml.ml](src/miniml.ml): the main entry point of
  our interpreter application (parses command line parameters and the
  input file, runs the input program and outputs the result, error
  reporting)

* [src/tests.ml](src/tests.ml): module with unit tests for
  your code using
  the [OUnit framework](http://ounit.forge.ocamlcore.org/).

The interpreter is almost completely functional. However, it misses
three functions that are only implemented as stubs (see below). These
functions are found in the module `Ast`. The file `ast.ml` is the only
file that you need to edit to complete this part of the
homework. Though, we also encourage you to add additional unit tests
to `tests.ml` for testing your code.


The root directory of the repository contains a shell
script [`build.sh`](build.sh) that you can use to compile
your code. Simply execute

```bash
./build.sh
```

and this will compile everything. You will have to install
`ocamlbuild` and `ounit` for this to work. Follow the [instructions in the class
notes for Class 7](https://github.com/nyu-pl-fa18/class07#installation-build-tools-and-ides) to do this. Assuming there are no compilation
errors, this script will produce two executable files in the root
directory:

* `miniml.native`: the executable of the MiniML interpreter.

* `tests.native`: the executable for running the unit tests.

You can find some test inputs for the interpreter in the directory `tests`.
In order to run the interpreter, execute e.g.

```bash
./miniml.native tests/test01.ml
```

Note that the interpreter will initially fail with an error message
`"Not yet implement"` when you run it on some of the tests because you
first need to implement the missing functions described below.

The interpreter supports the option `-v` which you can use to get some
additional debug output. In particular, once you have implemented the
pretty printer for MiniML, using the interpreter with this option will
print the input program on standard output after it has been parsed.

To run the unit tests, simply execute
```bash
./tests.native
```

The root directory of the repository also contains a
file [`.merlin`](.merlin), which is used by
the [Merlin toolkit](https://github.com/ocaml/merlin) to provide
editing support for OCaml code in various editors and IDEs. Assuming
you have set up an editor or IDE with Merlin, you should be able to
just open the source code files and start editing. Merlin should
automatically highlight syntax and type errors in your code and
provide other useful functionality. You may need to run `build.sh`
once so that Merlin is able to resolve the dependencies between the
different source code files.

### Part 1: MiniML Pretty Printer (6 Points)

Your first task is to implement a pretty printer for MiniML. That is,
you need to implement the two functions

```ocaml
string_of_term: term -> string

print_term: out_channel -> term -> unit
```

in `ast.ml`. The function `string_of_term` takes the AST of a MiniML
term as input and is supposed to convert it into its textual
representation. E.g. the code

```ocaml
let t =
  Lambda ("x", BinOp (Mult, IntConst (2, dummy_pos),
                      BinOp (Add, Var ("x", dummy_pos), IntConst (3, dummy_pos), dummy_pos),
                      dummy_pos), dummy_pos)
in
string_of_term t
```

should yield the string

```ocaml
fun x -> 2 * (x + 3)
```

Here, we use `dummy_pos` as a place-holder value for the source code
position of each subterm.

You are allowed to have additional redundant parenthesis in the output
string as in

```ocaml
(fun x -> (2 * (x + 3)))
```

In general, your implementation of the function `string_of_term`
should satisfy the following specification: for all terms `t`

```
equal t (Parser.parse_from_string (string_of_term t)) = true
```

Here, the predefined function `Parser.parse_from_string` parses the
string representation of a MiniML term and produces its AST
representation as a value of type `term`. The function `equal` is
defined in `ast.ml` and checks whether two MiniML terms are
structurally equal when one ignores the source code position
tags. Thus, the above requirement expresses that pretty printing a
term represented as an AST and parsing it back to an AST, yields the
same AST (modulo source code positions). The file `tests.ml`
contains several unit tests that uses this condition to test your
implementation. We encourage you to add more test cases there.

The second function `print_term`, which you also need to implement,
should print the string representation of the given term to the given
output channel. E.g. calling

```ocaml
print_term stdout t
```

will print the term `t` to standard output.

A straightforward implementation of these two functions is to
implement `string_of_term` by building the string representation of
the term recursively from its parts using string concatenation (`^`),
and then implement `print_term` using `string_of_term` and one of the
functions provided by the
module
[`Printf`](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html) in
the OCaml standard library. However, this is both inefficient and
nonidiomatic, and therefore strongly discouraged.

Instead, use
OCaml's
[Format](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html) module
to implement the pretty printer by implementing a function:

```ocaml
pr_term: Format.formatter -> term -> unit
```

The function `pr_term` can then be used to implement both
`string_of_term` and `print_term`. These implementations are already
provided for you.

Study how the `Format` module works. Challenge yourself and try to
produce nicely formatted output that uses the functionality of
`Format` to automatically add line breaks and indentation in your
output as needed. Also, try to minimize the number of parenthesis that
you produce in your output by taking advantage of operator precedence
and associativity. You will find the predefined functions `precedence`
and `is_left_associative` in the `Ast` module helpful for this. You
may even want to reintroduce `let` bindings in your generated output
to make it easier to read.

**Hint:** [this tutorial](https://ocaml.org/learn/tutorials/format.html)
discusses how to use the `Format` module and provides an
example towards the end that solves a very similar
problem than the one you have to implement here. So use that tutorial
to get started.

### Part 2: Finding a Free Variable in a Term (4 Points)

Your second task is to implement a function `find_free_var` that,
given a term `t`, checks whether `t` contains a free variable and if
so, returns that variable together with its source code position. We
use an `option` type to capture the case that `t` may not contain any
free variables:

```ocaml
find_free_var: term -> (var * pos) option
```

You can find some unit tests for this function in `tests.ml`.

The main function in `miniml.ml` uses the function `find_free_var` to
check whether all the variables occurring in the input program given
to the interpreter are properly declared and if not, produce an
appropriate error message.

Hint: you might find the functions provided by the module `Opt` in
`util.ml` helpful when implementing `find_free_var` (especially, the
functions `Opt.or_else` respectively `Opt.lazy_or_else`).

### Part 3: Implementing Substitution for Beta Reduction (5 Points)

Your final task for this problem is to implement a function

```ocaml
subst: term -> var -> term -> term
```

such that for given terms `t` and `s` and variable `x`,

```ocaml
subst t x s
```

computes the term `t[s/x]` obtained from `t` by substituting all free
occurrences of `x` in `t` by `s`. This function is used by the
interpreter in module `Eval` to evaluate function applications via
beta-reduction.

Since our interpreter will only evaluate closed terms (i.e. terms that
have no free variables), the interpreter maintains the invariant that
the terms `t` and `s` on which `subst` will be called are also
closed. This significantly simplifies the implementation of `subst` as
we do not have to account for alpha-renaming of bound variables within
`t` to avoid capturing free variables in `s` (since there are no free
variables that could be captured).

Some unit tests are already provided for you. Write additional unit
tests on your own. In addition, you can run the interpreter on the
input programs in [`tests/`](tests/) to make sure that it works as
expected. Feel free to write more input programs yourself and use them
for further testing.

