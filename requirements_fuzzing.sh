sudo apt install --yes afl++
opam switch create 4.12.0+afl --package=ocaml-variants.4.12.0+options,ocaml-option-afl
opam switch 4.12.0+afl
eval $(opam env)
opam install --yes . --deps-only
opam install crowbar bun