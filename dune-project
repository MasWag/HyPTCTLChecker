(lang dune 3.6)

(generate_opam_files true)

(name HyPTCTLChecker)

(using menhir 2.0)

(package
 (name HyPTCTLChecker)
 (depends
  (ocaml
   (>= 4.08))
   dune))
