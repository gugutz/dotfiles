################################
## emacs
################################

echo " ## installing Emacs" | boxes
# sudo pacman -Sq --noconfirm emacs
echo "making emacs start as a systemd unit service (do not run this line as root)" | boxes
systemctl --user enable --now emacs
echo "run emacs once for it to build PDF Tools server (required at first run)"
emacs --debug-init
echo "install ttf-all-the-icons from aur"
echo "this cant be `--noconfirm` because it replaces a lib and requires confirmation"
yay -S ttf-all-the-icons

echo "## all the icons"
yay -S emacs-all-the-icons

echo "install ag searcher for dumb-jump to work better"
sudo pacman -S --noconfirm the_silver_searcher

echo "## install typescript/javascript language server"
npm install -g typescript-language-server

echo "## install vscode-html-languageserver"
npm i -g vscode-html-languageserver-bin

echo "## install eslint"
npm install -g eslint babel-eslint eslint-plugin-react
echo "## install eslint typescript plugin"
npm install -g @typescript-eslint/eslint-plugin@latest

echo "## install angular language server"
npm install -g @angular/language-service@next typescript  @angular/language-server

echo "## install css language server"
npm install --global vscode-css-languageserver-bin

echo "## install pandoc for document generation in org mode"
sudo pacman -S --noconfirm pandoc

echo "## install Hunspell dictionaries for spellchecking with multiple dicts in emacs"
sudo pacman -S --noconfirm hunspell-pt_br
sudo pacman -S --noconfirm hunspell-en_US
