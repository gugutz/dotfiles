
#!/bin/bash

DOTFILES="~/dotfiles"
DOTFILES_BACKUP="~/dotfiles/backup"
# colors for echo commands using tput command
# color must be reseted at the end of the line or the entire file will use previous used color
red=`tput setaf 1`
green=`tput setaf 2`
reset_color=`tput sgr0`
bg_white=`tput setab 7`


# LANGUAGES CONFIG
echo "%{green}############################################################"
echo "## install ASDF"
echo "############################################################${reset_color}"
DIR=~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
if [ ! -d "$DIR" ]; then
    echo "## installing asdf"
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.7.6
else
    echo "asdf was already found in this system on $DIR"
fi

echo "%{green}############################################################"


echo "#checking if git is installed"
if git --version 2>&1 >/dev/null ; then
    echo "## installing git"
    sudo apt install -y git 1>/dev/null
else
    echo "git is already installed."
fi

echo "### install x tools"
echo "${green}## install xclip${reset_color}"
sudo apt install -y xclip 1>/dev/null
echo "# xorg props, used on a lot of different things. important!"
sudo apt install -y xorg-xprops 1>/dev/null
echo "# xorg xwininfo to find out information about windows and instances. # also very important."
sudo apt install -y xorg-xwininfo 1>/dev/null
sudo apt install -y xorg-xsetroot 1>/dev/null

echo "Replacements for the default GNU Linux tools"

sudo apt install -y bat 1>/dev/null
sudo apt install -y ncdu 1>/dev/null


echo "## installing set of terminal ASCII tools to complement terminal appearance"
sudo apt install -y neofetch 1>/dev/null
sudo apt install -y screenfetch 1>/dev/null
sudo apt install -y linux_logo 1>/dev/null
sudo apt install -y cmatrix 1>/dev/null
sudo apt install -y figlet 1>/dev/null
sudo apt install -y cowsay 1>/dev/null
sudo apt install -y sl 1>/dev/null

sudo apt install -y bash-pipes 1>/dev/null
sudo apt install -y toilet 1>/dev/null
sudo apt install -y boxes 1>/dev/null
sudo apt install -y beep 1>/dev/null

echo "## installing libwebp"
sudo apt install -y webp

echo "## installing gotop"
sudo apt install -y gotop



echo "%{green}############################################################"
echo "## Configuring terminal (st)"
echo "############################################################${reset_color}"
~/dotfiles/terminal/setup.sh

echo "%{green}############################################################"
echo "## Configuring shells"
echo "############################################################${reset_color}"

echo "## bash"
~/dotfiles/bash/setup.sh

echo "## zsh"
~/dotfiles/zsh/setup.sh

echo "%{green}############################################################"
echo "## Configuring tmux"
echo "############################################################${reset_color}"
~/dotfiles/tmux/setup.sh

echo "%{green}############################################################"
echo "## Configuring rofi"
echo "############################################################${reset_color}"
~/dotfiles/rofi/setup.sh

echo "%{green}############################################################"
echo "## Configuring vim"
echo "############################################################${reset_color}"
~/dotfiles/vim/setup.sh

echo "%{green}############################################################"
echo "## Configuring i3"
echo "############################################################${reset_color}"
~/dotfiles/emacs.d/setup.sh


# SEARCH TOOLS

sudo apt install -y the_silver_searcher 1>/dev/null
sudo apt install -y ripgrep 1>/dev/null


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


echo "%{green}############################################################"
echo "## setting up JavaScript environment"
echo "############################################################${reset_color}"


echo "## NVM"
if [ ! -d "~/.nvm" ]; then
    echo "## installing NVM"
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.2/install.sh | bash
else
    echo "nvm is already installed in ~/.asdf"
fi
echo "${green}## installing latest lts version of node and npm${reset_color}"
# nvm install --lts 1>/dev/null
# nvm use --lts
# nvm alias default node
nvm install 10.9.0
nvm use 10.9.0
nvm alias default 10.9.0


echo "%{green}## exporting NVM_DIR env var${reset_color}"
echo "%{green}## this is done because this variable is only exported by profile on next login${reset_color}"
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

echo "%{green}## refreshing SHELL variable${reset_color}"
exec $SHELL

echo "%{green}## installing yarn package manager${reset_color}"
curl -s https://raw.githubusercontent.com/tophat/yvm/master/scripts/install.js | node

echo "%{green}echo ## installing pure prompt for zsh${reset_color}"
npm install --global pure-prompt

echo "%{green}echo ## download and install Hack typeface for programming${reset_color}"
mkdir -p ~/temp
mkdir -p ~/.fonts/hack-ttf
wget https://github.com/source-foundry/Hack/releases/download/v3.003/Hack-v3.003-ttf.zip -O ~/temp
tar -xvc ~/temp/Hack-v3.*.zip -C ~/.fonts
rm -f ~/temp/Hack-v3.*.zip
echo "##updating font cache"
fc-cache -vf

echo "%{green}## installing angular cli${reset_color}"
npm list @angular/cli 1>/dev/null || npm install -g @angular/cli 1>/dev/null

echo "%{green}## installing ionic${reset_color}"
npm list ionic 1>/dev/null || npm install -g ionic 1>/dev/null


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



echo "## xcape for keyboard modifications"
sudo pacman --noconfirm --needed xcape

echo "## installing fontpreview for terminal utility"
yay --noconfirm --needed fontpreview

echo "%{green}############################################################"
echo "## qualirede things"
echo "############################################################${reset_color}"

echo "## Installing Google Chat electron wrapper"
sudo apt install -y google-chat-linux-git

echo "## Installing clockify time tracker"
sudo apt install -y clockify-desktop
