(lang dune 3.5)

(name PFPL_interpreters)

(generate_opam_files true)

(source
 (github LesleyLai/PFPL-Interpreters))

(authors "Lesley Lai")

(maintainers "Lesley Lai")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name PFPL_interpreters)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends base ocaml dune (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
