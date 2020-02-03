# using stdout redirect 1>/dev/null so only error messages will be shown

# colors for echo commands using tput command
# color must be reseted at the end of the line or the entire file will use previous used color
red=`tput setaf 1`
green=`tput setaf 2`
reset_color=`tput sgr0`
bg_white=`tput setab 7`



echo "${green}############################################################"
echo "## install xclip"
echo "############################################################${reset_color}"
sudo pacman -S --noconfirm xclip 1>/dev/null

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
echo "## install Ruby"
echo "############################################################${reset_color}"
asdf plugin-add ruby
asdf install ruby 2.6.4
asdf global ruby 2.6.4


echo "%{green}############################################################"
echo "## installing NVM"
echo "############################################################${reset_color}"
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.2/install.sh | bash
echo "${green}## installing latest lts version of node and npm${reset_color}"
nvm install --lts 1>/dev/null
nvm use --lts
nvm alias default node

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
npm install -g @angular/cli

echo "%{green}## installing ionic${reset_color}"
npm install -g ionic

echo "%{green}############################################################"
echo "## Configuring terminal (st)"
echo "############################################################${reset_color}"
~/dotfiles/st/setup.sh

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
