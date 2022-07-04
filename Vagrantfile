# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
    config.vm.boot_timeout = 600

    
    config.vm.box = "debian/bullseye64"
  
    # Configure virtual machine specs. Keep it simple, single user.
    config.vm.provider :virtualbox do |p|
        p.name = "Lg4DC_vardac"
        p.memory = 2048 
        p.cpus = 4 
    end
  
  
    # Config hostname and IP address so entry can be added to HOSTS file
    config.vm.hostname = "vagrant"
    config.vm.network :private_network, ip: '192.168.56.100'
  
    config.vm.synced_folder ".", "/vagrant", id: "vagrant-root"
    config.vm.provision :shell, path: "vagrant_bootstrap.sh"
    #config.vm.provision :docker
    #config.vm.provision :docker_compose
  end