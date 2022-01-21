#!/bin/bash

#TODO specify version
opam switch 4.13.1
opan install --yes dune=2.9.1 
opam install --yes yaml ocamlgraph ounit2 process rresult pprint ppx_deriving ppx_import menhir bos fileutils dune fieldslib ppx_fields_conv easy_logging jingoo dune-site

sudo apt install jc
pip3 install jinja-cli
