(lang dune 3.1)

(using menhir 2.1)

(name brawn)

(generate_opam_files true)

(source
 (github ayakayorihiro/cs6120-project))

(authors "Big Red")

(maintainers "Big Red")

(license LICENSE)

(documentation https://github.com/ayakayorihiro/cs6120-project/README)

(package
 (name brawn)
 (synopsis "An LLVM frontend for Awk.")
 (description "An LLVM frontend for Awk.")
 (depends llvm ocaml dune menhir))
