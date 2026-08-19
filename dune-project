(lang dune 3.10)

(name joy)

(generate_opam_files true)

(source
 (github Sudha247/ocaml-joy))

(authors "Sudha Parimala" "Kaustubh Maske Patil")

(maintainers "Sudha Parimala" "Kaustubh Maske Patil")

(license MIT)

(package
 (name joy-core)
 (synopsis "Core shapes, transforms, and noise primitives for ocaml-joy")
 (description "Pure OCaml core for the ocaml-joy creative coding library: shapes, transforms, colors, and simplex noise. Backend-agnostic.")
 (depends ocaml))

(package
 (name joy)
 (synopsis "Tiny creative coding library in OCaml - SVG/browser backend")
 (description "Tiny creative coding library in OCaml, targeting the browser via js_of_ocaml and SVG output.")
 (depends ocaml joy-core js_of_ocaml js_of_ocaml-ppx))

(package
 (name joy-cairo)
 (synopsis "Cairo backend for the ocaml-joy creative coding library")
 (description "Native Cairo backend for ocaml-joy, producing PNG output via the cairo2 bindings.")
 (depends ocaml joy-core cairo2))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
