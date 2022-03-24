# CS6120 Final Project Proposal 

## Who
@5hubh4m, @ayakayorihiro, @anshumanmohan

## What

We will implement a LLVM frontend implementation for the programming language `awk`. `awk` is a scripting language used to process text, and is a standard utility feature in many operating systems.

## How
// To do so, we will...

## Success?

An important but unquantifiable goal of this project is didactic; we'd like to teach ourselves LLVM via immersion.

A more quantifiable set of goals is:
- correctness: we will include a test suite to increase confidence in our implementation. 
- langauge features: we will compare our implementation with other existing compiler implementations of `awk`-like languages, including [lawk](http://lawk.sourceforge.net/) and [frawk](https://github.com/ezrosent/frawk). Each of these projects has made tradeoffs in what they support, and we will no doubt have to do the same.
- performance: we will compare our work against the above projects from a runtime perspective.