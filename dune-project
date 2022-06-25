(lang dune 2.8)
(using dune_site 0.1)
(name vardac)

(using menhir 2.1)

(generate_opam_files true)

(source (github severus21/vardac))
(license CECILL-2.1)
(authors "Laurent Prosperi")
(maintainers "laurent.prosperi@inria.fr")
(package
 (name vardac)
 (synopsis "TODO")
 (description "TODO")
 (depends
  (ocaml (>= 4.11.0))
  (menhir (>= 2.1))
 )
 (sites (share test_dataset) (share test_examples) (share externals) (share templates) (share akka_interfaces_grpc_externals) (share akka_interfaces_grpc_templates))
)
