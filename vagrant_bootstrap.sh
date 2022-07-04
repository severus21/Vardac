#!/bin/bash

sudo apt update 
./requirements.sh

# Setup Ocaml tool chain
sudo add-apt-repository -yes ppa:avsm/ppa
sudo apt update
sudo apt install opam --yes pkg-config


#echo "eval `opam config env`" >> ~/.bashrc
su vagrant && cd /vagrant && \
    opam init --auto-setup -vv --compiler=4.12.0 && \
    opam config env && \
    echo "test -r /home/$(whoami)/.opam/opam-init/init.sh && . /home/$(whoami)/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" >> /home/$(whoami)/.profile \
    echo "source /home/$(whoami)/.profile" >> /home/$(whoami)/.bashrc

# Install packages requirements for vardac
su vagrant && cd /vagrant && opam install --yes . --deps-only

# Complete
echo ""
echo "Vagrant install complete."
echo "Now try logging in:"
echo "    $ vagrant ssh"