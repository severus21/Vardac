#!/bin/bash

sudo apt update 

# Install git for version control, pip for install python packages
echo 'Installing git, Python 3, vim and pip...'
sudo apt-get -qq install git vim python3 python3-dev python3-pip python3-venv > /dev/null 2>&1


# Install ansible
echo 'Installing ansible'
echo "deb http://ppa.launchpad.net/ansible/ansible/ubuntu bionic main" | sudo tee /etc/apt/sources.list.d/ansible.list
apt -y install gnupg2
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 93C4A3FD7BB9C367
apt update
apt install --yes ansible 

# Complete
echo ""
echo "Vagrant install complete."
echo "Now try logging in:"
echo "    $ vagrant ssh"