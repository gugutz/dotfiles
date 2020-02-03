echo "################################"
echo "## zsh setup"
echo "## framework: oh-my-zsh"
echo "################################"

DOTFILES="$HOME/dotfiles"
echo "###############################"
echo "creating symbolic link to configuration file"

if [ -e $HOME/.zshrc ]; then
    mv -v  $HOME/.zshrc $HOME/.zshrc-bkp
fi
ln -s $DOTFILES/zsh/zshrc ~/.zshrc



echo "## installing zsh shell"
sudo pacman -Sq --noconfirm --needed zsh

echo "## pure prompt for zsh"
PURE_PROMPT=~/.zsh/pure
if [ ! -d "$PURE_PROMPT" ]; then
    echo "## installing pure prompt for zsh"
    git clone https://github.com/sindresorhus/pure.git ${HOME}/.zsh/pure
    # npm install --global pure-prompt
else
    echo "## pure-prompt already present in $PURE_PROMPT"
fi


echo "###############################"
echo "## OH-MY-ZSH FRAMEWORK"
OH_MY_ZSH=~/.oh-my-zsh
if [ ! -d "$OH_MY_ZSH" ]; then
    echo "## installing oh-my-zsh framework"
    curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
else
    echo "Oh My Zsh is already installed in $OH_MY_ZSH"
fi


echo "###############################"
echo "## OH-MY-ZSH Plugins"

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
