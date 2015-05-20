Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/trusty64"

  config.ssh.forward_x11 = true

  config.vm.provider "virtualbox" do |vb|
    vb.memory = 2048
    vb.cpus = 8
  end

  config.vm.provision "shell", inline: <<-SHELL
    echo '#{File.read("#{Dir.home}/.ssh/id_rsa.pub")}' \
      >>'/home/vagrant/.ssh/authorized_keys'

    # After installing docker, vagrant can't mount the shared /vagrant on next
    # boot. Likely a kernel change, which apt-get dist-upgrade solves.
    # So doing update and dist-upgrade before docker and a dist-upgrade after.
    # Just to be safe.
    sudo apt-get -y update
    sudo apt-get -y dist-upgrade
    wget -qO- https://get.docker.com/ | sh
    sudo apt-get -y dist-upgrade
  SHELL
end
