(lang dune 2.8)
(using dune_site 0.1)
(name compspec)

(using menhir 2.1)

(generate_opam_files true)

(source (github severus21/compspec))
(license CECILL-2.1)
(authors "Laurent Prosperi")
(maintainers "laurent.prosperi@inria.fr")
(package
 (name compspec)
 (synopsis "TODO")
 (description "TODO")
 (depends
  (ocaml (>= 4.11.0))
  (menhir (>= 2.1))
 )
 (sites (share externals) (share templates) (lib plugins))
)
