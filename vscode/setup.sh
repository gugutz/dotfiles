echo "################################"
echo "## vscode setup"
echo "##"
echo "################################"

DOTFILES="$HOME/dotfiles"
echo "###############################"
echo "creating symbolic link to configuration file"

if [ -e $HOME/.vscode ]; then
    mv -v  $HOME/.vscode $HOME/.vscode-bkp
fi
ln -s $DOTFILES/vscode ~/.vscode

if [ -e $HOME/.config/Code ]; then
    mv -v  $HOME/.config/Code $HOME/.config/Code-bkp
fi
ln -s $DOTFILES/vscode/code ~/.config/Code


echo "## install extensions from extensions.list"
cat vscode-extensions.list | xargs -L 1 code --install-extension
