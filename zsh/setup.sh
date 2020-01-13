echo "################################"
echo "## zsh setup"
echo "## terminal: st"
echo "## shell: zsh"
echo "## framework: oh-my-zsh"
echo "################################"

echo "## installing zsh shell"
sudo pacman -Sq --noconfirm zsh

echo "## installing st suckless terminal"
yay -S --noconfirm st
mkdir -p ~/downloads/st-0.8.2/
echo "## entering directory to download st stuff"
cd ~/downloads/st-0.8.2/
echo "## downloading st"
wget -P ~/downloads/st-0.8.2/ https://dl.suckless.org/st/st-0.8.2.tar.gz
tar -xfv ~/downloads/st-0.8.2/st-0.8.2.tar.gz --directory ~/downloads/st-0.8.2/
echo "## downloading and applying scrollback patch (Shift+PgUp/PgDown)"
wget -P ~/downloads/st-0.8.2/ https://st.suckless.org/patches/scrollback/st-scrollback-0.8.2.diff
patch -Np1 -i st-scrollback-0.8.2.diff
echo "## downloading and applying mouse scrollback patch (Shift+WheelUp/WheelDown)"
wget -P ~/downloads/st-0.8.2/ https://st.suckless.org/patches/scrollback/st-scrollback-mouse-0.8.2.diff
patch -Np1 -i st-scrollback-mouse-0.8.2.diff
echo "## downloading and applying clipboard patch"
wget -P ~/downloads/st-0.8.2/ https://st.suckless.org/patches/clipboard/st-clipboard-0.8.2.diff
patch -Np1 -i st-clipboard-0.8.2.diff
echo "## downloading and installing rightclickpaste patch"
wget -P ~/downloads/st-0.8.2/ https://st.suckless.org/patches/rightclickpaste/st-rightclickpaste-0.8.2.diff
patch -Np1 -i st-rightclickpaste-0.8.2.diff

echo "## installing oh-my-zsh framework"
curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
echo "## installing zsh sintax highlighting plugin"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
echo "## installing zsh autosuggestions plugin"
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
echo "## installing you-should-use aliases reminder plugin"
git clone https://github.com/MichaelAquilina/zsh-you-should-use.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/you-should-use
echo "## installing fzf command line fuzzy search"
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install
git clone https://github.com/supercrabtree/k ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/k


echo "## install ASDF"
git clone https://github.com/asdf-vm/asdf.git ~/.asdf

echo "## install Ruby"
asdf plugin-add ruby
asdf install ruby 2.6.4
asdf global ruby 2.6.4

echo "## installing Tmux"
sudo pacman -S --noconfirm tmux
echo "## install tmuxinator"
gem install tmuxinator

echo "## installing NVM"
curl -o https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash
echo "## installing latest lts version of node and npm"
nvm install --lts

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


wget -O ~/downloads/android-studio.tar.gz https://dl.google.com/dl/android/studio/ide-zips/3.5.1.0/android-studio-ide-191.5900203-linux.tar.gz?hl=es-419
