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
