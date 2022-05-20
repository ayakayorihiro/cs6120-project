+++
title = "An LLVM Frontend for a Small Programming Language"
[[extra.authors]]
name = "Shubham Chaudhary"
link = "https://www.cs.cornell.edu/~shubham/"
[[extra.authors]]
name = "Anshuman Mohan"
link = "https://www.cs.cornell.edu/~amohan/"
[[extra.authors]]
name = "Ayaka Yorihiro"
link = "https://ayakayorihiro.github.io"
+++

# Introduction

We report on `BRAWN`, the Big Red AWk iNterpreter. Our project is in service of `AWK`, which is a scripting language used to process text in a filter style. We build a new `LLVM` frontend for `AWK`, and then leverage `LLVM`'s backend to generate optimized machine code. 

# Implementation

## Overview
Here we skate over the parts of our implementation that are standard. We discuss more interesting issues in dedicated subsections.

Our plan is fairly standard:
1. Parse `AWK` into `OCaml`.
2. Write a `C++` runtime module that provides the built-in operations that `AWK` promises to support but which are not standard in LLVM. Compile this module to `LLVM` IR.
3. From the `AWK` AST in `OCaml`, Emit `LLVM` IR code. This code contains `extern` calls to the built-in operations.
4. Linking the two pieces above gives us `LLVM` IR code without gaps. Optimize this in `LLVM`.

Here's how we do it:
1. We use `Menhir`, which takes a grammar in the `yacc` format and generates a parser for `OCaml`. We choose `Menhir`, and therefore `OCaml`, because one of the officially-released grammars for `AWK` is written in `yacc`.
2. The mechanics of this step are standard: just ask `clang++` to emit LLVM. This is also where we run into many gotchas and make many of our design decisions. We discuss these in future sections.
3. This work is in `OCaml`. We walk over the AST generated previously and define an `LLVM` IR production for each constructor. Loosely speaking, there are three kinds of tasks:
    * issue straightforward calls to the `OCaml`-to-`LLVM` module
    * issue `extern` calls to the runtime module created previously
    * make basic blocks and design control flow that is accurate to `AWK`'s behavior
4. TK: gotta do it first

## Curiosities of `AWK`
1. `AWK` is dynamically typed
2. `AWK` is typically interpreted and not compiled
3. Certain built-in functions can be called with variable number of arguments, and they have somewhat unusual behaviour when this happens. For example, 
    * `length ()` succeeds: it gives the length of the argument $0. 
4. User-defined functions can be called with fewer arguments than they require. The variables that don't have arguments at call time are given default values.
5. `a = regex` assignment example... TK
6. Supporting `AWK`'s syntax of `BEGIN`/`END` blocks requires custom control flow. The same is true of the `next` and `exit` commands

## Points of Divergence (BRAWN vs AWK)
For convenience, we restrict ourselves to a (large) subset of `AWK`. Here are the points of divergence:
1. We reduce printing functionality
2. We do not offer output redirection
3. We restrict inputs to those coming from the command line
4. We require that all statement blocks be "terminated" TK
5. We require that all functions `return`

## Curiosities of Our Implementation
1. A lot of the fancy footwork happens at the interface between our built-in library and our codegen'd IR. One of our design choices is to have much of the heavy-lifting to the built-in library. For instance, even the `main()` function---which reads in inut, splits it into lines and then into words, and then runs the `AWK` program on it---lives in the runtime library. 
2. `AWK` uses the token `/` to indicate both `div` and the beginning/end of a regular expression. This is hard to parse, so in `BRAWN` we begin regular expressions with `/#` and end them with `#/`.

# Evaluation

## Correctness
For a selection of `awk` programs that we can process, we compare our output to that of the GNU `AWK` implementation, `gawk`. 

TK: selection criteria
TK: Turnt? other test-management suite?

## Benchmark
For the same programs as above, we provide a brief benchmark against `gawk`. 

TK

# References

1. The official guide to implementing a language using LLVM: https://releases.llvm.org/8.0.0/docs/tutorial/OCamlLangImpl1.html
2. An OCaml module for generating LLVM IR: https://llvm.moe/
3. Menhir: http://gallium.inria.fr/~fpottier/menhir/
4. The AWK specification: https://pubs.opengroup.org/onlinepubs/009604499/utilities/awk.html
5. Benchmarks obtained from https://github.com/ezrosent/frawk/blob/master/info/performance.md and https://www.math.utah.edu/docs/info/gawk_toc.html#SEC154
TK: once the order of these is stable, cite these above