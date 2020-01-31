#!/bin/bash

DOTFILES="~/dotfiles"
DOTFILES_BACKUP="~/dotfiles/backup"

echo "#checking if git is installed"
if git --version 2>&1 >/dev/null ; then
    echo "## installing git"
    sudo pacman -Sq --noconfirm --needed git 1>/dev/null
else
    echo "git is already installed."
fi

echo "### install x tools"
echo "## install xclip"
sudo pacman -S --noconfirm --needed xclip 1>/dev/null
echo "# xorg props, used on a lot of different things. important!"
sudo pacman -S --noconfirm --needed xorg-xprops 1>/dev/null
echo "# xorg xwininfo to find out information about windows and instances. # also very important."
sudo pacman -S --noconfirm --needed xorg-xwininfo 1>/dev/null
sudo pacman -S --noconfirm --needed xorg-xsetroot 1>/dev/null

echo "Replacements for the default GNU Linux tools"

sudo pacman -S --noconfirm --needed bat 1>/dev/null
sudo pacman -S --noconfirm --needed ncdu 1>/dev/null


echo "## installing set of terminal ASCII tools to complement terminal appearance"
sudo pacman -S --noconfirm --needed neofetch 1>/dev/null
sudo pacman -S --noconfirm --needed screenfetch 1>/dev/null
sudo pacman -S --noconfirm --needed linux_logo 1>/dev/null
sudo pacman -S --noconfirm --needed cmatrix 1>/dev/null
sudo pacman -S --noconfirm --needed figlet 1>/dev/null
sudo pacman -S --noconfirm --needed cowsay 1>/dev/null
sudo pacman -S --noconfirm --needed sl 1>/dev/null

yay -S --noconfirm --needed bash-pipes 1>/dev/null
yay -S --noconfirm --needed toilet 1>/dev/null
yay -S --noconfirm --needed boxes 1>/dev/null
yay -S --noconfirm --needed beep 1>/dev/null

echo "## installing gotop"
yay -S --noconfirm --needed gotop

# LANGUAGES CONFIG
echo "###############################"
echo "## ASDF"
if [ ! -d "~/.asdf" ]; then
    echo "## install asdf"
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.7.6
else
    echo "ASDF is already installed in ~/.asdf"
fi


echo "###############################"
echo "## install Ruby"
asdf plugin-add ruby
asdf install ruby 2.6.4
asdf global ruby 2.6.4

echo "# Pry enhanced irb with colors and indentation"
gem install pry

echo "# install rails"
gem install rails

echo "## Ruby installed"


echo "###############################"
echo "## setting up JavaScript environment"


if [ ! -d "~/.nvm" ]; then
    echo "## installing NVM"
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.2/install.sh | bash
else
    echo "nvm is already installed in ~/.asdf"
fi
echo "## installing latest lts version of node and npm"
nvm install --lts
nvm use --lts
nvm alias default node

echo "## refreshing SHELL variable"
# exec $SHELL

echo "## YARN version manager"
if [ ! -d "~/.yvm" ]; then
    echo "## installing yarn version manager"
    curl -s https://raw.githubusercontent.com/tophat/yvm/master/scripts/install.js | node
else
    echo "yvm is already installed in ~/.yvm"
fi


echo "## Installing Hack Font"
if [ ! -d "~/.fonts/hack-ttf" ]; then
    echo "## download and install Hack typeface for programming"
    mkdir -p ~/temp
    mkdir -p ~/.fonts/hack-ttf
    wget https://github.com/source-foundry/Hack/releases/download/v3.003/Hack-v3.003-ttf.zip -O ~/temp
    tar -xvc ~/temp/Hack-v3.*.zip -C ~/.fonts
    rm -f ~/temp/Hack-v3.*.zip
    echo "##updating font cache"
    fc-cache -vf
else
    echo "## Hack font is already installed in ~/.fonts/hack-ttf"
fi

echo "## installing angular cli"
npm list @angular/cli 1>/dev/null || npm install -g @angular/cli 1>/dev/null

echo "## installing ionic"
npm list ionic 1>/dev/null || npm install -g ionic 1>/dev/null
