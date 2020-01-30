
echo "# install bash completion"
sudo pacman -S --noconfirm bash-completion

echo "# creating symbolic links"
ln -sf ./bashrc ~/.bashrc
ln -sf ./bash_profile ~/.bash_profile
ln -sf ./profile ~/.profile
