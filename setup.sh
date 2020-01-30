
echo "## install xclip"
sudo pacman -S --noconfirm xclip

# LANGUAGES CONFIG
echo "## install ASDF"
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.7.6

echo "## install Ruby"
asdf plugin-add ruby
asdf install ruby 2.6.4
asdf global ruby 2.6.4


echo "## installing NVM"
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.2/install.sh | bash
echo "## installing latest lts version of node and npm"
nvm install --lts

echo "## refreshing SHELL variable"
exec $SHELL

echo "## installing yarn package manager"
curl -s https://raw.githubusercontent.com/tophat/yvm/master/scripts/install.js | node

echo "## installing pure prompt for zsh"
npm install --global pure-prompt

echo "## download and install Hack typeface for programming"
mkdir -p ~/temp
mkdir -p ~/.fonts/hack-ttf
wget https://github.com/source-foundry/Hack/releases/download/v3.003/Hack-v3.003-ttf.zip -O ~/temp
tar -xvc ~/temp/Hack-v3.*.zip -C ~/.fonts
rm -f ~/temp/Hack-v3.*.zip
echo "##updating font cache"
fc-cache -vf

echo "## installing angular cli"
npm install -g @angular/cli

echo "## installing ionic"
npm install -g ionic


