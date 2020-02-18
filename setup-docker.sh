
if [ -f "/etc/debian_version" ]; then
    # Uninstall old versions
    sudo apt-get remove docker docker-engine docker.io containerd runc

    # Update the apt package index:
    sudo apt update

    # Install packages to allow apt to use a repository over HTTPS:
    sudo apt-get install \
         apt-transport-https \
         ca-certificates \
         curl \
         gnupg-agent \
         software-properties-common

    # Add Docker’s official GPG key:
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

    # Verify that you now have the key with the fingerprint 9DC8 5822 9FC7 DD38 854A E2D8 8D81 803C 0EBF CD88, by searching for the last 8 characters of the fingerprint.
    sudo apt-key fingerprint 0EBFCD88
    #
    # Expected output:
    #
    # pub   rsa4096 2017-02-22 [SCEA]
    # 9DC8 5822 9FC7 DD38 854A  E2D8 8D81 803C 0EBF CD88
    # uid           [ unknown] Docker Release (CE deb) <docker@docker.com>
    # sub   rsa4096 2017-02-22 [S]fi


    # Setup docker stable repository
    sudo add-apt-repository \
         "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
    $(lsb_release -cs) \
    stable"

    # Install docker-ce
    sudo apt-get install docker-ce docker-ce-cli containerd.io
fi

if [ -f "/etc/arch-release" ]; then
    sudo pacman -S --noconfirm --needed docker
fi

# Post-installation steps for Linux

# Create the docker group.
sudo groupadd docker

# Add your user to the docker group.
sudo usermod -aG docker $USER

# Activate the changes to groups:
newgrp docker

# Configure Docker to start on boot
sudo systemctl enable docker


# Install docker compose
sudo curl -L "https://github.com/docker/compose/releases/download/1.25.3/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose

# Apply executable permissions to the binary:
sudo chmod +x /usr/local/bin/docker-compose
