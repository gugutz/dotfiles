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

echo "install ag searcher for dumb-jump to work better"
sudo pacman -S --noconfirm the_silver_searcher

echo "## install typescript/javascript language server"
npm install -g typescript-language-server

echo "## install eslint"
npm install -g eslint babel-eslint eslint-plugin-react


echo "## install angular language server3"
wget -o angular-language-server.zip https://marketplace.visualstudio.com/_apis/public/gallery/publishers/Angular/vsextensions/ng-template/0.802.3/vspackage
tar -xvf angular-language-server --directory ~/.angular

echo "## install pandoc for document generation in org mode"
sudo pacman -S --noconfirm pandoc
