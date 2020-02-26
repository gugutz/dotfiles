DOTFILES_DIR="~/dotfiles"
DOTFILES_BACKUP_DIR="~/dotfiles/backup"
mkdir -p #DOTFILES_BACKUP_DIR
echo "## install xwininfo to use the find-window-criteria script"
sudo pacman -S --noconfirm --needed xorg-xwininfo

echo "## install i3blocks blocks to /usr/lib/i3blocks"
sudo pacman -S --noconfirm --needed i3blocks
yay -S --noconfirm --needed i3blocks-contrib

echo "## install hsetroot for solid colors wallpapers"
sudo pacman -S --noconfirm --needed hsetroot

echo "## install nitrogen"
sudo pacman -S --noconfirm --needed nitrogen

echo "## install feh for screenshots and gif wallapers"
sudo pacman -S --noconfirm --needed feh

echo "## install xscreensaver"
sudo pacman -S --noconfirm --needed xscreensaver


# i3 setup
if [ -e $HOME/.i3/config ]; then
    mv -v  $HOME/i3/config $DOTFILES_BACKUP_DIR/i3.config.bkp
fi
ln -sf $DOTFILES_DIR/i3/config $HOME/.i3/config
