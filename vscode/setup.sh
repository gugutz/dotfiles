echo "################################"
echo "## vscode setup"
echo "##"
echo "################################"

DOTFILES="$HOME/dotfiles"
echo "###############################"

if [ -e $HOME/.vscode ]; then
    echo "Backing up current existing config"
    mv -v  $HOME/.vscode $HOME/.vscode-bkp
else
    echo "Creating settings folder in ~/.config/Code"
    mkdir -p ~/.config/Code
fi

echo "creating symbolic link to configuration file"
ln -sf $DOTFILES/vscode/keybindings.json ~/.config/Code/User/keybindings.json
ln -sf $DOTFILES/vscode/settings.json ~/.config/Code/User/settings.json

echo "## install extensions from extensions.list"
cat extensions.list | xargs -L 1 code --install-extension
