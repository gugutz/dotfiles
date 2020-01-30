################################
## emacs
################################

echo " ## installing Emacs"
# sudo pacman -Sq --noconfirm emacs
echo "making emacs start as a systemd unit service (do not run this line as root)"
systemctl --user enable --now emacs

echo "install ttf-all-the-icons (required by emacs-all-the-icons"
echo "this cant be --noconfirm because it replaces a lib and requires confirmation"
#yay -S ttf-all-the-icons

echo "## all the icons"
yay -S --noconfirm emacs-all-the-icons

echo "install ag searcher for dumb-jump to work better"
sudo pacman -S --noconfirm the_silver_searcher

#### LANGUAGE SERVERS
echo "## install typescript/javascript language server"
npm install -g typescript-language-server

echo "## install angular language server"
npm install -g @angular/language-service@next typescript  @angular/language-server

echo "## install css language server"
npm install --global vscode-css-languageserver-bin

echo "## install vscode-html-languageserver"
npm i -g vscode-html-languageserver-bin

echo "## installing yaml language server"
npm install -g yaml-language-server

echo "## installing bash language server"
npm i -g bash-language-server
#################################

echo "## install eslint"
npm install -g eslint babel-eslint eslint-plugin-react

echo "## install eslint typescript plugin"
npm install -g @typescript-eslint/eslint-plugin@latest

echo "## install html-tidy"
sudo pacman -S --noconfirm tidy

echo "## installing bash language server"
npm i -g bash-language-server

#echo "## install pandoc for document generation in org mode"
#sudo pacman -S --noconfirm pandoc

#echo "## install Hunspell dictionaries for spellchecking with multiple dicts in emacs"
#sudo pacman -S --noconfirm hunspell-pt_br
#sudo pacman -S --noconfirm hunspell-en_US

echo " ## creating symbolic links for the configuration"
ln -sf ~/dotfiles/emacs.d ~/.emacs.d
