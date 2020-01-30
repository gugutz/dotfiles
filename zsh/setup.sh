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
tar -xvf ~/downloads/st-0.8.2/st-0.8.2.tar.gz --directory ~/downloads/
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
echo "## downloading and installing alpha (transparency) patch"
wget -P ~/downloads/st-0.8.2/ https://st.suckless.org/patches/alpha/st-alpha-0.8.2.diff
patch -Np1 -i st-alpha-0.8.2.diff

# OH-MY-ZSH FRAMEWORK
echo "Oh-My-ZSH"
OH_MY_ZSH=~/.oh-my-zsh
if [ ! -d "$OH_MY_ZSH" ]; then
    echo "## installing oh-my-zsh framework"
    curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
else 
    echo "Oh My Zsh is already installed in $OH_MY_ZSH"
fi


# PLUGINS

echo "Syntax Highlighting"
SYNTAX_HIGHLIGHTING=~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
if [ ! -d "$SYNTAX_HIGHLIGHTING" ]; then
    echo "## installing zsh sintax highlighting plugin"
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
else 
    echo "Plugin Auto Suggestions already installed in $SYNTAX_HIGHLIGHTING"
fi

echo "Auto Suggestions"
AUTO_SUGGESTIONS=~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
if [ ! -d "$AUTO_SUGGESTIONS" ]; then
    echo "## installing zsh autosuggestions plugin"
    git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
else 
    echo "Plugin Auto Suggestions already installed in $AUTO_SUGGESTIONS"
fi

echo "You Should Use"
YOU_SHOULD_USE=~/.oh-my-zsh/custom/plugins/you-should-use
if [ ! -d "$YOU_SHOULD_USE" ]; then
echo "## installing you-should-use aliases reminder plugin"
git clone https://github.com/MichaelAquilina/zsh-you-should-use.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/you-should-use
else 
    echo "Plugin You Should Use already installed in $YOU_SHOULD_USE"
fi


echo "FZF"
FZF=~/.fzf
if [ ! -d "$FZF" ]; then
    echo "## installing fzf command line fuzzy search"
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install
else 
    echo "Plugin You Should Use already installed in $FZF"
fi

echo "K"
K=~/.oh-my-zsh/custom/plugins/k
if [ ! -d "$K" ]; then
    echo "## installing k directory list enhancements"
    git clone https://github.com/supercrabtree/k ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/k
else 
    echo "Plugin K already installed in $K"
fi


echo "creating symbolic link to configuration file"
ln -sf ./zshrc ~/.zshrc


