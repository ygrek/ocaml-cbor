(lang dune 2.7)

(implicit_transitive_deps false)

(license MIT)
(authors "ygrek")
(maintainers "ygrek@autistici.org")
(source (github ygrek/ocaml-cbor))
(bug_reports "https://github.com/ygrek/ocaml-cbor/issues")
(homepage "https://github.com/ygrek/ocaml-cbor")
(documentation "https://github.com/ygrek/ocaml-cbor")

(generate_opam_files true)

(package
 (name cbor)
 (tags ("org:ygrek"))
 (synopsis "CBOR encoder/decoder (RFC 7049) - native OCaml implementation")
 (description "
The Concise Binary Object Representation (CBOR) is a data format whose design
goals include the possibility of extremely small code size, fairly small
message size, and extensibility without the need for version negotiation.
")
 (depends
  (ocaml (>= 4.02))
  base-bytes
  (ocplib-endian (>= 0.6))
  (yojson (and :with-test (>= 1.6)))))
