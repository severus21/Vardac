#!/bin/bash

sudo apt update
sudo apt install --yes jq python3 python3-pip
sudo pip3 install -r requirements.txt

sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt update
sudo apt install opam dune --yes pkg-config

opam init --auto-setup -vv --compiler=4.12.0
opam config env
echo "test -r /home/$(whoami)/.opam/opam-init/init.sh && . /home/$(whoami)/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" >> /home/$(whoami)/.profile
echo "source /home/$(whoami)/.profile" >> /home/$(whoami)/.bashrc

opam install --yes . --deps-only