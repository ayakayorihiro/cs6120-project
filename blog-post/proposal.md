# CS 6120 Final Project Proposal

## Who

[@5hubh4m](github.com/5hubh4m), [@ayakayorihiro](github.com/ayakayorihiro), and [@anshumanmohan](github.com/anshumanmohan)

## What

We will implement an LLVM frontend for a small but useful programming language. The choices we are considering are:

- `awk`, which is a scripting language used to process text, and is a standard utility feature in many operating systems; and
- the DSL within `jq`, a popular JSON processing tool.

We wanted a language that is *simple* enough that tasks like parsing do not become the majority of work, while still *useful* enough that substantial programs can (and have been) written in it. To this end, we may implement a convenient subset of a language that fulfils our criteria. We are primarily interested in `awk`, and are listing `jq` as a fallback.

## How

LLVM has excellent facilities for writing language frontends including [this incredible tutorial](https://releases.llvm.org/13.0.0/docs/tutorial/index.html). We will write a frontend for our target and then use LLVM's backend to generate optimized machine code.

## How We Will Measure Success

An important but unquantifiable goal of this project is didactic; we would like to teach ourselves LLVM via immersion.

A more quantifiable set of goals is:

- **Correctness.** We will include a test suite to increase confidence in our implementation. 
- **Features.** We will compare our implementation with other existing compiler implementations of our language.

    For `awk` that would be [gawk](https://www.gnu.org/software/gawk/), the GNU implementation, [mawk](https://invisible-island.net/mawk/), an interpreter for `awk` with a focus on speed, [lawk](http://lawk.sourceforge.net/), an LLVM implementation of `awk` and [frawk](https://github.com/ezrosent/frawk), another LLVM implementation that has advanced features like automatic parallelization.
    
    For `jq` it might just be the [official implementation](https://stedolan.github.io/jq/).
    
- **Performance.** We will compare our work against the above projects from a runtime perspective on reasonable benchmarks.
