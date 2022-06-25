#!/bin/bash

sudo apt update 

# Setup Ocaml tool chain
sudo add-apt-repository -yes ppa:avsm/ppa
sudo apt update
sudo apt install opam --yes pkg-config


su vagrant && opam init --auto-setup -vv --compiler=4.12.0
su vagrant && opam config env
#echo "eval `opam config env`" >> ~/.bashrc
su vagrant && echo "test -r /home/$(whoami)/.opam/opam-init/init.sh && . /home/i$(whoami)/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" >> ~/.profile
su vagrant && echo "source /home/$(whoami)/.profile" >> ~/.bashrc

# Install packages requirements for vardac
su vagrant && cd /vagrant
su vagrant && ./requirements.sh

# Complete
echo ""
echo "Vagrant install complete."
echo "Now try logging in:"
echo "    $ vagrant ssh"