#!/bin/bash

#TODO specify version
opam install --yes yaml ocamlgraph ounit2 process rresult pprint ppx_deriving ppx_import menhir bos fileutils dune fieldslib ppx_fields_conv easy_logging jingoo dune-site dune-release ppx_hash
opam install odoc

sudo apt install jc
pip3 install jinja-cli
