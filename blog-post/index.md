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

Our project is in service of `AWK`, which is a scripting language used to process text. We build a new `LLVM` frontend for `AWK`, and then leverage `LLVM`'s backend to generate optimized machine code.

# Implementation

## Overview
Here we will skate over the parts that are standard. We discuss more interesting issues in dedicated subsections.

Our plan is fairly standard:
1. Parse `AWK` into `OCaml`.
2. Write a `C++` runtime module that provides the built-in operations that `AWK` expects but which are not standard in LLVM. Compile this module to `LLVM` IR.
3. From the `AWK` AST in `OCaml`, Emit `LLVM` IR code. This code contains `extern` calls to the built-in operations.
4. Linking the two pieces above gives us `LLVM` IR code without gaps. Optimize this in `LLVM`.

Here's how we do it:
1. We use `Menhir`, which takes a grammar in the `yacc` format and generates a parser for `OCaml`. We choose `Menhir`, and therefore `OCaml`, because the one of the officially-released grammars for `AWK` is written in `yacc`.
2. The mechanics of this step are standard, but this is also where we run into many gotchas and make many of our design decisions. We discuss these in future sections.
3. This work is in `OCaml`. Essentially we walk over the AST generated previously and define an `LLVM` IR production for each constructor. Loosely speaking, there are three kinds of tasks:
    * issue straightforward calls to the `OCaml`-to-`LLVM` module
    * issue `extern` calls to the runtime module created previously
    * make basic blocks and design control flow
4. TK: gotta do it first

## Curiosities of AWK

## Points of Divergence (BRAWN vs AWK)

## Curiosities of Our Implementation


# Evaluation

While we initially proposed to benchmark our results against other implementations of LLVM, our primary focus has become correctness. 

TK: selection criteria
TK: Alcotest? other test-management suite?

# References

1. The official guide to implementing a language using LLVM: https://releases.llvm.org/8.0.0/docs/tutorial/OCamlLangImpl1.html
2. An OCaml module for generating LLVM IR: https://llvm.moe/
3. Menhir: http://gallium.inria.fr/~fpottier/menhir/
4. The AWK specification: https://pubs.opengroup.org/onlinepubs/009604499/utilities/awk.html
5. Benchmarks obtained from https://github.com/ezrosent/frawk/blob/master/info/performance.md and https://www.math.utah.edu/docs/info/gawk_toc.html#SEC154
