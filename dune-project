(lang dune 3.1)

(name brawn)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name brawn)
 (synopsis "An LLVM frontend for Awk.")
 (description "An LLVM frontend for Awk.")
 (depends ocaml dune menhir))
