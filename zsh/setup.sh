echo "################################"
echo "## zsh setup"
echo "## terminal: st"
echo "## shell: zsh"
echo "## framework: oh-my-zsh"
echo "################################"

echo "## installing zsh shell"
pacman -Sq --noconfirm zsh

echo "## installing oh-my-zsh framework"
curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh

echo "## installing zsh sintax highlighting plugin"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

echo "## installing zsh autosuggestions plugin"
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

echo "## installing fzf command line fuzzy search"
$ git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install


echo "## install ASDF"
git clone https://github.com/asdf-vm/asdf.git ~/.asdf
asdf plugin-add ruby
asdf install ruby 2.6.4
asdf global ruby 2.6.4

echo "## installing Tmux"
sudo pacman -S --noconfirm tmux
echo "## install tmuxinator"
gem install tmuxinator

echo "## installing NVM"
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash
echo "## installing latest lts version of node and npm"
nvm install --lts

echo "## installing yarn package manager"
curl -s https://raw.githubusercontent.com/tophat/yvm/master/scripts/install.js | node

echo "## installing pure prompt for zsh"
$ npm install --global pure-prompt

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
