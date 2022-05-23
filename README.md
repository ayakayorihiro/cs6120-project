# Big Red AWk implementatioN (BRAWN)

This is a project for CS 6120 at Cornell University (Spring 2022).
We aim to implement an LLVM frontend for a subset of the Awk
programming language.

## Building and Running

This project depends on GNU `make`, Ocaml, and LLVM. On macOS those can
be installed with `brew`.

```
brew install opam llvm make autoconf automake libtool
opam init
opam switch create 4.12.0
opam install llvm menhir ppx_deriving
```

Then go to the project directory and 

```
cd cs6120-project/
make
```

To compile a `.brawn` file

```
cd cs6120-project/
./brawn input-file.brawn output-executable
```

NOTE: More details on the [CS 6120 Blog Post](blog-post/index.md).
