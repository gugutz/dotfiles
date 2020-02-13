############################################################
# docker

echo "Uninstall old versions of docker"
sudo apt-get remove docker docker-engine docker.io containerd runc

echo "Installing docker dependencies"
sudo apt-get install \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common


echo "Add docker official GPG key"
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

echo "Add docker repository"
sudo add-apt-repository \
    "deb [arch=amd64] https://download.docker.com/linux/ubuntu ${V} stable"

echo "Verifying fingerprint ending with 0EBFCD88"
sudo apt-key fingerprint 0EBFCD88


echo "Update the apt package index"
sudo apt-get update


echo "Install docker engine"
sudo apt-get install docker-ce docker-ce-cli containerd.io

echo "Creating the docker group"
sudo groupadd docker

echo "Adding current user to the docker group"
sudo usermod -aG docker $USER

echo "Activating recent changes to groups"
newgrp docker

echo "Enabling docker service to load at startup"
sudo systemctl enable docker

############################################################
# docker compose
echo "Installing docker compose"
sudo curl -L "https://github.com/docker/compose/releases/download/1.25.3/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose

echo "Apply executable permissions to docker compose executable"
sudo chmod +x /usr/local/bin/docker-compose

echo "Testing the installation"
docker-compose --version

