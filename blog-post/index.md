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

Our project is in service of the `awk` programming language,
which is a scripting language used to process text.
We build a new `LLVM` frontend for `awk`, and then leverage `LLVM`'s backend 
to generate optimized machine code.


# Implementation

## Overview
Here we present our work in broad strokes.
We will skate over the parts that are standard, and 
will disucss the more interesting issues below.

We follow a straightforward recipe:
1. Parse `awk` into `OCaml`.
2. Emit `LLVM` IR code, with `extern` calls to the built-in operations that `awk` expects.
3. Write a `C++` runtime module that provides the built-in operations. Compile this to `LLVM` IR.
4. Link the `LLVM` IR code generated to the compiled runtime. This is now `LLVM` IR code without gaps.
5. Optimize the above in `LLVM`.

## Curiosities of Awk

## Points of Divergence

## Curiosities of Our Implementation


# Evaluation


# References

1. The official guide to implementing a language using LLVM: https://releases.llvm.org/8.0.0/docs/tutorial/OCamlLangImpl1.html
2. A set of OCaml bindings for generating LLVM IR: https://llvm.moe/
