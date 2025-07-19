(lang dune 3.16)

(using menhir 3.0)

(name perk)

(generate_opam_files true)

(source
 (github Alex23087/Perk))

(authors "Alex23087" "mell-o-tron")

(maintainers "Alex23087" "mell-o-tron")

(license LICENSE)

(documentation https://perklang.org/docs/perk/Perk/index.html)

(package
 (name perk)
 (synopsis "A modern, ergonomic, low level programming language.")
 (description
  "Perk is a modern low level programming language, compiled to C. It builds on C's simplicity, adding features such as option types, lambdas and typeclasses. Its current goal is to replace C in the implementation of the MellOS operating system.")
 (depends ocaml dune cmdliner)
 (tags
  ("programming language" "compiler" "lambda functions" "low-level")))
