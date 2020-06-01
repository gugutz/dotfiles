echo "################################"
echo "## vscode setup"
echo "##"
echo "################################"

DOTFILES="$HOME/dotfiles"
echo "###############################"


echo "## install extensions from extensions.list"
cat extensions.list | xargs -L 1 code --install-extension
