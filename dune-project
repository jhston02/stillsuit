(lang dune 3.13)

(name stillsuit)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name stillsuit)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune bitv (alcotest :with-test) (qcheck :with-test) (qcheck-alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
