(lang dune 3.10)

(name watchbox)

(generate_opam_files true)

(source
 (github frectonz/watchbox))

(authors "frectonz")

(maintainers "frectonz")

(license MIT)

(package
 (name watchbox)
 (synopsis "My personal program for watching movies and shows")
 (depends ocaml dune notty nottui feather utop))
