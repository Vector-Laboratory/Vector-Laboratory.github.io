(lang dune 3.21)
(using menhir 3.0)

(name arra)

(generate_opam_files true)

(package
 (name arra)
 (synopsis "Arra language compiler")
 (depends
  (ocaml (>= 5.0.0))
  zarith
  menhir
  sexplib
  ppx_sexp_conv
  (qcheck-core (and :with-test (>= 0.91)))))
